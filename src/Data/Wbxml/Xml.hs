{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Data.Wbxml.Xml
    ( DecoderData(..)
    , EncoderData(..)
    , Decoder
    , Encoder
    , mkDecoderData
    , mkEncoderData
    , runDecoder
    , runEncoder
    , evalEncoder
    , toXmlString
    , toXmlEntity
    , toXmlName
    , toXmlAttribute
    , toXmlElement
    , toXmlInstruction
    , toXmlNode
    , toXmlDocument
    , fromXmlString
    , fromXmlName
    , fromXmlAttribute
    , fromXmlElement
    , fromXmlInstruction
    , fromXmlNode
    , fromXmlDocument
    , segmentString
    ) where

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Except (Except, runExcept, throwE)
import           Control.Monad.Trans.Reader (ReaderT, asks, runReaderT)
import           Control.Monad.Trans.State  (StateT, evalStateT, gets, modify,
                                             runStateT)

import           Data.ByteString            (ByteString)
import           Data.Text                  (Text)
import           Data.Word                  (Word32)

import           Control.Arrow              (second)
import           Data.Char                  (chr)
import           Data.List                  (sortOn)
import           Data.Maybe                 (catMaybes)

import qualified Data.ByteString            as BS
import qualified Data.Map                   as Map
import qualified Data.Text                  as Text
import qualified Data.Trie                  as Trie
import qualified Data.XML.Types             as XML

import           Data.Wbxml.DocumentType
import           Data.Wbxml.Types

data DecoderData = DecoderData { decoderDoctype    :: DocumentType
                               , decoderDecodeText :: ByteString -> Except String Text
                               , decoderStrTable   :: StrTable
                               }

data EncoderData = EncoderData { encoderDoctype      :: DocumentType
                               , encoderEncodeText   :: Text -> Except String ByteString
                               , encoderStrTable     :: Map.Map ByteString Word32
                               , encoderStrTableSize :: Word32
                               }

type Decoder = ReaderT DecoderData (Except String)

type Encoder = StateT EncoderData (Except String)

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(.:) = (.).(.)

fromJust :: e -> Maybe a -> Except e a
fromJust err = maybe (throwE err) return

mkDecoderData :: DocumentType -> (ByteString -> Except String Text) -> StrTable -> DecoderData
mkDecoderData = DecoderData

mkEncoderData :: DocumentType -> (Text -> Except String ByteString) -> EncoderData
mkEncoderData dt encode = EncoderData dt encode Map.empty 0

runDecoder :: Decoder a -> DecoderData -> Either String a
runDecoder = runExcept .: runReaderT

runEncoder :: Encoder a -> EncoderData -> Either String (a, EncoderData)
runEncoder = runExcept .: runStateT

evalEncoder :: Encoder a -> EncoderData -> Either String a
evalEncoder = runExcept .: evalStateT

decodeText :: ByteString -> Decoder Text
decodeText bs = asks decoderDecodeText >>= (\f -> lift $ f bs)

encodeText :: Text -> Encoder ByteString
encodeText t = gets encoderEncodeText >>= (\f -> lift $ f t)

decodeStringLookup :: Word32 -> Decoder Text
decodeStringLookup idx = do
  res <- asks (lookupStr idx . decoderStrTable)
  bs <- lift $ fromJust "string reference out of bounds" res
  decodeText bs

decodeElementName :: CodepageReference -> Decoder Text
decodeElementName cr = do
  res <- asks (Map.lookup cr . docElements . decoderDoctype)
  lift $ fromJust "unknown element codepage reference" res

decodeAttributeStart :: CodepageReference -> Decoder (Text, Text)
decodeAttributeStart cr = do
  res <- asks (Map.lookup cr . docAttributeStarts . decoderDoctype)
  lift $ fromJust "unknown attribute start codepage reference" res

decodeAttributeValue :: CodepageReference -> Decoder Text
decodeAttributeValue cr = do
  res <- asks (Map.lookup cr . docAttributeValues . decoderDoctype)
  lift $ fromJust "unknown attribute value codepage reference" res

decodeAttrSeq :: AttributeStart -> [AttributeValue] -> Decoder (Text, Text)
decodeAttrSeq start values = do
  (name, first) <- decodeAttrStart start
  rest <- mapM decodeAttrValue values
  return (name, Text.concat (first : rest))
    where
      decodeAttrStart v = case v of
                            TaggedAttributeStart cr -> decodeAttributeStart cr
                            LiteralAttributeStart idx -> split <$> decodeStringLookup idx
      decodeAttrValue v = case v of
                            TaggedAttributeValue cr -> decodeAttributeValue cr
                            StrAttributeValue str -> toXmlString str
                            ExtensionAttributeValue _ -> lift $ throwE "Decoding WBXML Extension Tokens not supported"
                            EntityAttributeValue e -> toXmlEntity e
      split = second (Text.drop 1) . Text.breakOn "="

toXmlString :: Str -> Decoder Text
toXmlString s = case s of
                  Inline bs -> decodeText bs
                  Reference i -> decodeStringLookup i

toXmlEntity :: Entity -> Decoder Text
toXmlEntity (Entity n) = return $ Text.singleton (chr (fromIntegral n))

toXmlName :: ElementName -> Decoder XML.Name
toXmlName name = do
  str <- case name of
           TaggedElementName cr -> decodeElementName cr
           LiteralElementName idx -> decodeStringLookup idx
  return $ XML.Name str Nothing Nothing

toXmlAttribute :: Attribute -> Decoder (XML.Name, [XML.Content])
toXmlAttribute Attribute{..} = do
  (name, value) <- decodeAttrSeq attributeStart attributeValues
  return (XML.Name name Nothing Nothing, [ XML.ContentText value ])

toXmlElement :: Element -> Decoder XML.Element
toXmlElement Element{..} = do
  name <- toXmlName elementName
  attributes <- mapM toXmlAttribute elementAttributes
  nodes <- mapM toXmlNode elementContents
  return $ XML.Element name attributes nodes

toXmlInstruction :: ProcessingInstruction -> Decoder XML.Instruction
toXmlInstruction ProcessingInstruction{..} = do
  (name, value) <- decodeAttrSeq processingStart processingValues
  return $ XML.Instruction name value

toXmlNode :: Content -> Decoder XML.Node
toXmlNode c =
    case c of
      ElementContent e -> XML.NodeElement <$> toXmlElement e
      StrContent s -> XML.NodeContent . XML.ContentText <$> toXmlString s
      ExtensionContent _ -> lift $ throwE "Decoding WBXML Extension Tokens not supported"
      EntityContent e -> XML.NodeContent . XML.ContentText <$> toXmlEntity e
      ProcessingInstructionContent i -> XML.NodeInstruction <$> toXmlInstruction i
      OpaqueContent bs -> XML.NodeContent . XML.ContentText <$> decodeText bs

toXmlDocument :: Document -> Decoder XML.Document
toXmlDocument Document{..} = do
  dt <- asks (fmap (flip XML.Doctype Nothing) . docDoctype . decoderDoctype)
  prologue <- pure $ XML.Prologue [] dt []
  root <- toXmlElement documentRoot
  epilogue <- pure []
  return $ XML.Document prologue root epilogue

encodeStringLookup :: ByteString -> Encoder Word32
encodeStringLookup bs = do
  word <- gets (Map.lookup bs . encoderStrTable)
  case word of
    Just n -> return n
    Nothing -> do
              size <- gets encoderStrTableSize
              modify (\s -> s { encoderStrTable = Map.insert bs size (encoderStrTable s)
                              , encoderStrTableSize = encoderStrTableSize s + fromIntegral (BS.length bs) + 1
                              })
              return size

encodeString :: ByteString -> Encoder Str
encodeString bs = do
  word <- gets (Map.lookup bs . encoderStrTable)
  case word of
    Just n -> return $ Reference n
    Nothing -> return $ Inline bs

encodeAttributeStart :: ByteString -> ByteString -> Encoder (AttributeStart, ByteString)
encodeAttributeStart n v = do
  let str = BS.concat [n, "=", v]
  trie <- gets (docRevAttributeStarts . encoderDoctype)
  case Trie.match trie str of
    Just (m, cr, rest) | BS.length m > BS.length n -> return (TaggedAttributeStart cr, rest)
    _ -> do
      i <- encodeStringLookup n
      return (LiteralAttributeStart i, v)

encodeAttributeValue :: ByteString -> Encoder [AttributeValue]
encodeAttributeValue str = do
  trie <- gets (docRevAttributeValues . encoderDoctype)
  mapM translate $ segmentString trie str
      where translate (bs, Nothing) = StrAttributeValue <$> encodeString bs
            translate (_, Just cr) = return $ TaggedAttributeValue cr

encodeAttrSeq :: Text -> Text -> Encoder (AttributeStart, [AttributeValue])
encodeAttrSeq name value = do
  bsname <- encodeText name
  bsvalue <- encodeText value
  (as, rest) <- encodeAttributeStart bsname bsvalue
  av <- encodeAttributeValue rest
  return (as, av)

fromXmlString :: Text -> Encoder Str
fromXmlString t = do
  bs <- encodeText t
  encodeString bs

fromXmlName :: XML.Name -> Encoder ElementName
fromXmlName XML.Name{..} = do
  bs <- encodeText nameLocalName
  cr <- gets (Map.lookup bs . docRevElements . encoderDoctype)
  case cr of
    Just cr' -> return $ TaggedElementName cr'
    Nothing -> LiteralElementName <$> encodeStringLookup bs

fromXmlAttribute :: (XML.Name, [XML.Content]) -> Encoder Attribute
fromXmlAttribute (name, values) = do
  value <- merge values
  (start, values') <- encodeAttrSeq (XML.nameLocalName name) value
  return $ Attribute start values'
    where
      merge vs = Text.concat <$> mapM unpack vs
      unpack (XML.ContentEntity _) = lift $ throwE "Encoding ContentEntity not supported"
      unpack (XML.ContentText t) = return t

fromXmlElement :: XML.Element -> Encoder Element
fromXmlElement XML.Element{..} = do
  name <- fromXmlName elementName
  attrs <- mapM fromXmlAttribute elementAttributes
  contents <- catMaybes <$> mapM fromXmlNode elementNodes
  return $ Element name attrs contents

fromXmlInstruction :: XML.Instruction -> Encoder ProcessingInstruction
fromXmlInstruction XML.Instruction{..} = do
  (start, values) <- encodeAttrSeq instructionTarget instructionData
  return $ ProcessingInstruction start values

fromXmlNode :: XML.Node -> Encoder (Maybe Content)
fromXmlNode (XML.NodeElement e) = Just . ElementContent <$> fromXmlElement e
fromXmlNode (XML.NodeContent (XML.ContentText t)) = Just . StrContent <$> fromXmlString t
fromXmlNode (XML.NodeContent (XML.ContentEntity _)) = lift $ throwE "Encoding ContentEntity not supported"
fromXmlNode (XML.NodeInstruction i) = Just . ProcessingInstructionContent <$> fromXmlInstruction i
fromXmlNode (XML.NodeComment _) = return Nothing

fromXmlDocument :: Header -> XML.Document -> Encoder Document
fromXmlDocument hdr XML.Document{..} = do
  root <- fromXmlElement documentRoot
  strtable <- makeStrTable =<< gets encoderStrTable
  return $ Document hdr strtable root
    where makeStrTable m = do
            let strs = map fst . sortOn snd . Map.toList $ m
            return $ StrTable . BS.concat . map (`BS.snoc` 0) $ strs

segmentString :: Trie.Trie a -> ByteString -> [(ByteString, Maybe a)]
segmentString _ "" = []
segmentString trie str = go $ zip (BS.inits str) (BS.tails str)
    where go [] = []
          go [(i, _)] = [ (i, Nothing) ]
          go ((i, t):l) = case Trie.match trie t of
                           Just (m, v, str') -> if BS.null i
                                                then (m, Just v) : segmentString trie str'
                                                else (i, Nothing) : (m, Just v) : segmentString trie str'
                           Nothing -> go l
