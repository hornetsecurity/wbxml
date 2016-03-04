module Data.Wbxml.Types
    ( CodepageReference(..)
    , StrTable(..)
    , Str(..)
    , Version(..)
    , PublicId(..)
    , Charset(..)
    , Entity(..)
    , Extension(..)
    , ElementName(..)
    , AttributeStart(..)
    , AttributeValue(..)
    , Attribute(..)
    , Element(..)
    , ProcessingInstruction(..)
    , Content(..)
    , Header(..)
    , Document(..)
    , lookupStr
    ) where

import           Data.ByteString ( ByteString )
import           Data.Word       ( Word32, Word8 )

import qualified Data.ByteString as BS

data CodepageReference =
      CodepageReference { crCodepage  :: Word8
                        , crReference :: Word8
                        }
    deriving (Eq, Show, Ord)

newtype StrTable = StrTable { unStrTable :: ByteString }
    deriving (Eq, Show)

data Str = Inline ByteString
         | Reference Word32
    deriving (Eq, Show)

data Version = Version { versionMajor :: Word8
                       , versionMinor :: Word8
                       }
    deriving (Eq, Show)

data PublicId = KnownPublicId Word32
              | LiteralPublicId Word32
    deriving (Eq, Show)

newtype Charset = Charset Word32
    deriving (Eq, Show)

newtype Entity = Entity Word32
    deriving (Eq, Show)

data Extension = Extension { extensionId    :: Word8
                           , extensionValue :: Maybe Str
                           }
    deriving (Eq, Show)

data ElementName = TaggedElementName CodepageReference
                 | LiteralElementName Word32
    deriving (Eq, Show)

data AttributeStart = TaggedAttributeStart CodepageReference
                    | LiteralAttributeStart Word32
    deriving (Eq, Show)

data AttributeValue = TaggedAttributeValue CodepageReference
                    | StrAttributeValue Str
                    | ExtensionAttributeValue Extension
                    | EntityAttributeValue Entity
    deriving (Eq, Show)

data Attribute = Attribute { attributeStart  :: AttributeStart
                           , attributeValues :: [AttributeValue]
                           }
    deriving (Eq, Show)

data Element = Element { elementName       :: ElementName
                       , elementAttributes :: [Attribute]
                       , elementContents   :: [Content]
                       }
    deriving (Eq, Show)

data ProcessingInstruction =
      ProcessingInstruction { processingStart  :: AttributeStart
                            , processingValues :: [AttributeValue]
                            }
    deriving (Eq, Show)

data Content = ElementContent Element
             | StrContent Str
             | ExtensionContent Extension
             | EntityContent Entity
             | ProcessingInstructionContent ProcessingInstruction
             | OpaqueContent ByteString
    deriving (Eq, Show)

data Header = Header { headerVersion  :: Version
                     , headerPublicId :: PublicId
                     , headerCharset  :: Charset
                     }
    deriving (Eq, Show)

data Document = Document { documentHeader   :: Header
                         , documentStrTable :: StrTable
                         , documentRoot     :: Element
                         }
    deriving (Eq, Show)

lookupStr :: Word32 -> StrTable -> Maybe ByteString
lookupStr index table = if BS.length bs > i
                        then Just $ BS.takeWhile (/= 0) $ BS.drop i bs
                        else Nothing
  where
    i = fromIntegral index
    bs = unStrTable table
