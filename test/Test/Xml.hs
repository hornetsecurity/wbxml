{-# LANGUAGE OverloadedStrings #-}

module Test.Xml ( testXml ) where

import           Data.Text               (Text)
import           Data.Word               (Word8)

import           Data.Either             (isLeft)
import           Data.Text.Encoding      (decodeUtf8, encodeUtf8)

import qualified Data.ByteString         as BS
import qualified Data.Trie               as Trie
import qualified Data.XML.Types          as XML

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Wbxml.DocumentType
import           Data.Wbxml.Types
import           Data.Wbxml.Xml

testSegmentString :: TestTree
testSegmentString =
    testCase "segmentString" $ do
      segmentString trie "" @?= []

      segmentString trie "foo" @?= [ ("foo", Just 0) ]
      segmentString trie "quux" @?= [ ("quux", Nothing) ]

      segmentString trie "foobarbaz" @?= [ ("foo", Just 0)
                                         , ("bar", Just 1)
                                         , ("baz", Just 2)
                                         ]

      segmentString trie "the foo and the bar" @?= [ ("the ", Nothing)
                                                   , ("foo", Just 0)
                                                   , (" and the ", Nothing)
                                                   , ("bar", Just 1)
                                                   ]

          where trie = Trie.fromList [ ("foo", 0 :: Int)
                                     , ("bar", 1)
                                     , ("baz", 2)
                                     ]

cp :: Word8 -> CodepageReference
cp = CodepageReference 0

name :: Text -> XML.Name
name n = XML.Name n Nothing Nothing

attr :: Text -> [Text] -> (XML.Name, [XML.Content])
attr n v = (name n, XML.ContentText <$> v)

decode :: Decoder a -> Either String a
decode = flip runDecoder $ mkDecoderData spec (return . decodeUtf8) strtable
    where spec = fromSpec specWml10
          strtable = StrTable $ BS.concat $ map (`BS.snoc` 0) [ "foo", "bar", "baz" ]

testToXmlString :: TestTree
testToXmlString =
    testCase "String" $ do
      decode (toXmlString $ Inline "foo") @?= Right "foo"
      decode (toXmlString $ Reference 4) @?= Right "bar"

      isLeft (decode (toXmlString $ Reference 12)) @? "invalid string reference should fail to decode"

testToXmlEntity :: TestTree
testToXmlEntity =
    testCase "Entity" $
      decode (toXmlEntity $ Entity 32) @?= Right " "

-- <a href="https://google.com/">
--  ^ ^^^^^^^^^^^^^^      ^^^^^
--  | \ 0x4c              \ 0x85
--  \ 0x1c

testToXmlName :: TestTree
testToXmlName =
    testCase "Name" $ do
      decode (toXmlName $ TaggedElementName (cp 0x1c)) @?= Right (name "a")
      decode (toXmlName $ LiteralElementName 0) @?= Right (name "foo")

testToXmlAttribute :: TestTree
testToXmlAttribute =
    testCase "Attribute" $ do
      decode (toXmlAttribute $ Attribute (TaggedAttributeStart (cp 0x4a)) []) @?= Right (attr "href" [ "" ])
      decode (toXmlAttribute $ Attribute (LiteralAttributeStart 0) []) @?= Right (attr "foo" [ "" ])

      decode (toXmlAttribute $ Attribute
                                 (TaggedAttributeStart (cp 0x4c))
                                 [ StrAttributeValue (Inline "google")
                                 , TaggedAttributeValue (cp 0x85)
                                 ])
                 @?= Right (attr "href" [ "https://google.com/" ])

testToXmlElement :: TestTree
testToXmlElement =
    testCase "Element" $ do
      decode (toXmlElement $ Element (TaggedElementName (cp 0x1c)) [] [])
                 @?= Right (XML.Element (name "a") [] [])

      decode (toXmlElement $ Element
                               (TaggedElementName (cp 0x1c))
                               [ Attribute (TaggedAttributeStart (cp 0x4a)) [] ]
                               [ StrContent (Reference 4) ])
                 @?= Right (XML.Element
                            (name "a")
                            [ attr "href" [ "" ] ]
                            [ XML.NodeContent (XML.ContentText "bar") ])

testToXmlInstruction :: TestTree
testToXmlInstruction =
    testCase "Instruction" $ do
      decode (toXmlInstruction $ ProcessingInstruction (TaggedAttributeStart (cp 0x21)) [])
             @?= Right (XML.Instruction "name" "")

      decode (toXmlInstruction $ ProcessingInstruction (LiteralAttributeStart 0) [ StrAttributeValue (Inline "lorem") ])
             @?= Right (XML.Instruction "foo" "lorem")

testToXmlNode :: TestTree
testToXmlNode =
    testCase "Node" $ do
      decode (toXmlNode $ ElementContent $ Element (TaggedElementName (cp 0x1c)) [] [])
             @?= Right (XML.NodeElement $ XML.Element (name "a") [] [])

      decode (toXmlNode $ StrContent $ Inline "lorem")
             @?= Right (XML.NodeContent $ XML.ContentText "lorem")

      isLeft (decode (toXmlNode $ ExtensionContent $ Extension 0 Nothing))
             @? "extensions are not supported"

      decode (toXmlNode $ EntityContent $ Entity 32)
             @?= Right (XML.NodeContent $ XML.ContentText " ")

      decode (toXmlNode $ ProcessingInstructionContent $ ProcessingInstruction (LiteralAttributeStart 0) [])
             @?= Right (XML.NodeInstruction $ XML.Instruction "foo" "")

      decode (toXmlNode $ OpaqueContent "lorem")
             @?= Right (XML.NodeContent $ XML.ContentText "lorem")

encode :: Encoder a -> Either String a
encode = flip evalEncoder $ mkEncoderData spec (return . encodeUtf8)
    where spec = fromSpec specWml10

testFromXmlString :: TestTree
testFromXmlString =
    testCase "String" $ do
      encode (fromXmlString "") @?= Right (Inline "")
      encode (fromXmlString "lorem ipsum") @?= Right (Inline "lorem ipsum")

testFromXmlName :: TestTree
testFromXmlName =
    testCase "Name" $ do
      encode (fromXmlName $ name "a") @?= Right (TaggedElementName (cp 0x1c))
      encode (fromXmlName $ name "foo") @?= Right (LiteralElementName 0)

testFromXmlAttribute :: TestTree
testFromXmlAttribute =
    testCase "Attribute" $ do
      encode (fromXmlAttribute $ attr "href" []) @?= Right (Attribute (TaggedAttributeStart (cp 0x4a)) [])
      encode (fromXmlAttribute $ attr "foo" []) @?= Right (Attribute (LiteralAttributeStart 0) [])

      encode (fromXmlAttribute $ attr "href" [ "https://google.com/" ])
                 @?= Right (Attribute (TaggedAttributeStart (cp 0x4c))
                                      [ StrAttributeValue (Inline "google")
                                      , TaggedAttributeValue (cp 0x85)
                                      ])

testFromXmlElement :: TestTree
testFromXmlElement =
    testCase "Element" $ do
      encode (fromXmlElement $ XML.Element (name "a") [] [])
                 @?= Right (Element (TaggedElementName (cp 0x1c)) [] [])

      encode (fromXmlElement $ XML.Element
                                 (name "a")
                                 [ attr "href" [ "https://google.com/" ] ]
                                 [ XML.NodeContent (XML.ContentText "bar") ])
             @?= Right (Element (TaggedElementName (cp 0x1c))
                                [ Attribute (TaggedAttributeStart (cp 0x4c)) [ StrAttributeValue (Inline "google")
                                                                             , TaggedAttributeValue (cp 0x85) ] ]
                                [ StrContent (Inline "bar") ])

testFromXmlInstruction :: TestTree
testFromXmlInstruction =
    testCase "Instruction" $ do
      encode (fromXmlInstruction $ XML.Instruction "name" "")
             @?= Right (ProcessingInstruction (TaggedAttributeStart (cp 0x21)) [])

      encode (fromXmlInstruction $ XML.Instruction "foo" "lorem")
             @?= Right (ProcessingInstruction (LiteralAttributeStart 0) [ StrAttributeValue (Inline "lorem") ])

testFromXmlNode :: TestTree
testFromXmlNode =
    testCase "Node" $ do
      encode (fromXmlNode $ XML.NodeElement $ XML.Element (name "a") [] [])
             @?= Right (Just (ElementContent $ Element (TaggedElementName (cp 0x1c)) [] []))

      encode (fromXmlNode $ XML.NodeContent $ XML.ContentText "lorem")
             @?= Right (Just (StrContent $ Inline "lorem"))
      isLeft (encode (fromXmlNode $ XML.NodeContent $ XML.ContentEntity "nbsp"))
             @? "entity content is not supported"

      encode (fromXmlNode $ XML.NodeInstruction $ XML.Instruction "foo" "")
             @?= Right (Just (ProcessingInstructionContent $ ProcessingInstruction (LiteralAttributeStart 0) []))

      encode (fromXmlNode $ XML.NodeComment "")
             @?= Right Nothing

testXml :: TestTree
testXml = testGroup "Data.Wbxml.Xml"
          [ testSegmentString
          , testGroup "toXml" [ testToXmlString
                              , testToXmlEntity
                              , testToXmlName
                              , testToXmlAttribute
                              , testToXmlElement
                              , testToXmlInstruction
                              , testToXmlNode
                              ]
          , testGroup "fromXml" [ testFromXmlString
                                , testFromXmlName
                                , testFromXmlAttribute
                                , testFromXmlElement
                                , testFromXmlInstruction
                                , testFromXmlNode
                                ]
          ]
