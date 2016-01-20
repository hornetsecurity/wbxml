{-# LANGUAGE RecordWildCards #-}

module Data.Wbxml.Printer
    ( PrinterState(..)
    , initialPrinterState
    , Printer
    , codepageSwitch
    , codepageReference
    , str
    , version
    , publicId
    , charset
    , opaque
    , entity
    , extension
    , attributeStart
    , attributeValue
    , attribute
    , element
    , processingInstruction
    , content
    , header
    , document
    , wbxmlDocument
    ) where

import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.State  (State, evalState, gets, modify)
import           Control.Monad.Trans.Writer (WriterT, execWriterT, tell)

import           Data.Bits                  (shiftL, shiftR, (.&.), (.|.))
import           Data.ByteString            (ByteString)
import           Data.ByteString.Builder    (Builder)
import           Data.Word                  (Word32, Word8)

import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as B

import           Data.Wbxml.Tokens
import           Data.Wbxml.Types           hiding (attributeStart)

data PrinterState = PrinterState { stateTagPage  :: Word8
                                 , stateAttrPage :: Word8
                                 } deriving (Show)

initialPrinterState :: PrinterState
initialPrinterState = PrinterState 0 0

type Printer = WriterT Builder (State PrinterState)

word8 :: Word8 -> Printer ()
word8 = tell . B.word8

word32 :: Word32 -> Printer ()
word32 w = mapM_ (word8 . fromIntegral) $ reverse $ toBytes w
    where toBytes n = if n > 127
                      then ((n .&. 0x7f) .|. 0x80) : toBytes (n `shiftR` 7)
                      else [n]

codepageSwitch :: Word8 -> Printer ()
codepageSwitch cp = word8 tokenSwitchPage >> word8 cp

codepageReference :: (PrinterState -> Word8) -> (Word8 -> PrinterState -> PrinterState) -> CodepageReference -> Printer ()
codepageReference g s (CodepageReference cp ref) = do
    cur <- lift $ gets g
    let switch = if cp /= cur then lift (modify (s cp)) >> codepageSwitch cp else return mempty
    switch >> word8 ref

attrCodepageReference :: CodepageReference -> Printer ()
attrCodepageReference = codepageReference stateAttrPage (\p s -> s { stateAttrPage = p })

tagCodepageReference :: CodepageReference -> Printer ()
tagCodepageReference = codepageReference stateTagPage (\p s -> s { stateTagPage = p })

lenBytes :: ByteString -> Printer ()
lenBytes bs = word32 (fromIntegral $ BS.length bs) >> tell (B.byteString bs)

termStr :: ByteString -> Printer ()
termStr bs = tell (B.byteString bs) >> word8 0

str :: Str -> Printer ()
str (Reference index) = word8 tokenStrT >> word32 index
str (Inline bs) = word8 tokenStrI >> termStr bs

version :: Version -> Printer ()
version Version{..} = word8 tok
    where tok = (((versionMajor - 1) `shiftL` 4) .&. 0xf0) .|. (versionMinor .&. 0x0f)

publicId :: PublicId -> Printer ()
publicId (KnownPublicId pid) = word32 pid
publicId (LiteralPublicId pid) = word32 0 >> word32 pid

charset :: Charset -> Printer ()
charset (Charset c) = word32 c

opaque :: ByteString -> Printer ()
opaque bs = word8 tokenOpaque >> lenBytes bs

entity :: Entity -> Printer ()
entity (Entity e) = word8 tokenEntity >> word32 e

extension :: Extension -> Printer ()
extension (Extension eid Nothing) = word8 (0xc0 .|. eid)
extension (Extension eid (Just (Reference i))) = word8 (0x80 .|. eid) >> word32 i
extension (Extension eid (Just (Inline bs))) = word8 (0x40 .|. eid) >> termStr bs

attributeStart :: AttributeStart -> Printer ()
attributeStart (TaggedAttributeStart cp) = attrCodepageReference cp
attributeStart (LiteralAttributeStart i) = word8 tokenLiteral >> word32 i

attributeValue :: AttributeValue -> Printer ()
attributeValue (TaggedAttributeValue cp) = attrCodepageReference cp
attributeValue (StrAttributeValue s) = str s
attributeValue (ExtensionAttributeValue e) = extension e
attributeValue (EntityAttributeValue e) = entity e

attribute :: Attribute -> Printer ()
attribute (Attribute start values) = attributeStart start >> mapM_ attributeValue values

element :: Element -> Printer ()
element (Element name attributes contents) = n >> attrs >> conts
    where
      mask = marker 0x80 attributes .|. marker 0x40 contents
      marker b l = if null l then 0x00 else b
      n = case name of
               TaggedElementName (CodepageReference cp i) -> tagCodepageReference (CodepageReference cp (i .|. mask))
               LiteralElementName i -> word8 (tokenLiteral .|. mask) >> word32 i
      attrs = list attribute attributes
      conts = list content contents
      list p l = if null l then return mempty else mapM_ p l >> word8 tokenEnd

processingInstruction :: ProcessingInstruction -> Printer ()
processingInstruction (ProcessingInstruction start values) =
    word8 tokenPi
    >> attributeStart start
    >> mapM_ attributeValue values
    >> word8 tokenEnd

content :: Content -> Printer ()
content (ElementContent e) = element e
content (StrContent s) = str s
content (ExtensionContent e) = extension e
content (EntityContent e) = entity e
content (ProcessingInstructionContent p) = processingInstruction p
content (OpaqueContent bs) = opaque bs

header :: Header -> Printer ()
header Header{..} = version headerVersion
                    >> publicId headerPublicId
                    >> charset headerCharset

document :: Document -> Printer ()
document Document{..} = header documentHeader
                        >> lenBytes (unStrTable documentStrTable)
                        >> element documentRoot

wbxmlDocument :: Document -> Builder
wbxmlDocument doc = evalState (execWriterT $ document doc) initialPrinterState
