module Data.Wbxml.Parser
    ( ParserState(..)
    , initialParserState
    , Parser
    , codepageSwitch
    , codepageReference
    , str
    , version
    , publicId
    , charset
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

import           Control.Monad.Trans.Class  ( lift )
import           Control.Monad.Trans.State  ( StateT, gets, modify, runStateT )

import           Data.Bits                  ( (.&.), (.|.), shiftL, shiftR )
import           Data.ByteString            ( ByteString )
import           Data.Word                  ( Word32, Word8 )

import qualified Data.Attoparsec.ByteString as AP

import           Data.Wbxml.Tokens
import           Data.Wbxml.Types           hiding ( attributeStart )

data ParserState = ParserState { stateTagPage  :: Word8
                               , stateAttrPage :: Word8
                               }
    deriving (Show)

initialParserState :: ParserState
initialParserState = ParserState 0 0

type Parser = StateT ParserState AP.Parser

token :: Word8 -> Parser Word8
token = lift . AP.word8

word8 :: Parser Word8
word8 = lift AP.anyWord8

word32 :: Parser Word32
word32 = go 0
  where
    go n = do
        x <- word8
        let n' = (n `shiftL` 7) .|. fromIntegral (x .&. 0x7f)
        if x .&. 0x80 == 0 then return n' else go n'

codepageSwitch :: (Word8 -> ParserState -> ParserState) -> Parser ()
codepageSwitch cp = switch >>= modify . cp
  where
    switch = token tokenSwitchPage *> word8

codepageReference :: (ParserState -> Word8)
                  -> AP.Parser Word8
                  -> Parser CodepageReference
codepageReference cp ref = CodepageReference <$> gets cp <*> lift ref

lenBytes :: Parser ByteString
lenBytes = word32 >>= lift . AP.take . fromIntegral

indexStr :: Parser Str
indexStr = Reference <$> word32

termStr :: Parser Str
termStr = Inline <$> lift (AP.takeWhile (/= 0)) <* token 0

str :: Parser Str
str = AP.choice [ token tokenStrI *> termStr, token tokenStrT *> indexStr ]

version :: Parser Version
version = do
    tok <- word8
    return $ Version (((tok .&. 0xf0) `shiftR` 4) + 1) (tok .&. 0x0f)

publicId :: Parser PublicId
publicId = do
    pid <- word8
    if pid == 0
        then LiteralPublicId <$> word32
        else KnownPublicId <$> return (fromIntegral pid)

charset :: Parser Charset
charset = Charset <$> word32

opaque :: Parser ByteString
opaque = token tokenOpaque *> lenBytes

entity :: Parser Entity
entity = token tokenEntity *> (Entity <$> word32)

extension :: Parser Extension
extension = do
    tok <- validToken
    let eid = fromIntegral $ tok .&. 0x03
    if (tok .&. 0xc0) == 0xc0
        then return $ Extension eid Nothing
        else if (tok .&. 0xc0) == 0x80
             then Extension eid . Just <$> indexStr
             else Extension eid . Just <$> termStr
  where
    validToken = lift $ AP.satisfy (\x -> (x .&. 0xc0) /= 0 && (x .&. 0x3f) < 3)

attributeStart :: Parser AttributeStart
attributeStart = AP.option () (codepageSwitch (\p s -> s { stateAttrPage = p }))
    *> AP.choice [ token tokenLiteral *> (LiteralAttributeStart <$> word32)
                 , TaggedAttributeStart <$> codepageReference stateAttrPage (AP.satisfy (< 128))
                 ]

attributeValue :: Parser AttributeValue
attributeValue = AP.option () (codepageSwitch (\p s -> s { stateAttrPage = p }))
    *> AP.choice [ StrAttributeValue <$> str
                 , ExtensionAttributeValue <$> extension
                 , EntityAttributeValue <$> entity
                 , TaggedAttributeValue <$> codepageReference stateAttrPage (AP.satisfy (>= 128))
                 ]

attribute :: Parser Attribute
attribute = Attribute <$> attributeStart <*> AP.many' attributeValue

element :: Parser Element
element = AP.option () (codepageSwitch (\p s -> s { stateTagPage = p })) *> do
    tok <- validToken
    let code = tok .&. 0x3f
    name <- if code == tokenLiteral
            then LiteralElementName <$> word32
            else TaggedElementName <$> codepageReference stateTagPage
                                                         (pure code)
    attrs <- if (tok .&. 0x80) /= 0 then attributes else return []
    conts <- if (tok .&. 0x40) /= 0 then contents else return []
    return $ Element name attrs conts
  where
    validToken = lift $ AP.satisfy (\x -> (x .&. 0x3f) >= 4)
    attributes = AP.manyTill attribute (token tokenEnd)
    contents = AP.manyTill content (token tokenEnd)

processingInstruction :: Parser ProcessingInstruction
processingInstruction = token tokenPi
    *> (ProcessingInstruction <$> attributeStart
                              <*> AP.manyTill attributeValue (token tokenEnd))

content :: Parser Content
content = AP.choice [ StrContent <$> str
                    , ExtensionContent <$> extension
                    , EntityContent <$> entity
                    , ProcessingInstructionContent <$> processingInstruction
                    , OpaqueContent <$> opaque
                    , ElementContent <$> element
                    ]

header :: Parser Header
header = Header <$> version <*> publicId <*> charset

document :: Parser Document
document = Document <$> header <*> (StrTable <$> lenBytes) <*> element

wbxmlDocument :: AP.Parser Document
wbxmlDocument = fst <$> runStateT document initialParserState
