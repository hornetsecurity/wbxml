{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Test.Parser ( testParser ) where

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as BS

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Wbxml.Parser
import           Data.Wbxml.Types

{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

testExample1 :: TestTree
testExample1 =
    testCase "Example 1" $
    case AP.parseOnly (wbxmlDocument <* AP.endOfInput) input of
      Left e -> assertFailure e
      Right Document{..} -> do
        let Header{..} = documentHeader

        headerVersion @?= Version 1 1
        headerPublicId @?= KnownPublicId 1
        headerCharset @?= Charset 3

        documentStrTable @?= StrTable ""

        elementName documentRoot @?= TaggedElementName (CodepageReference 0 7)
        elementAttributes documentRoot @?= []

        let [ ElementContent e ] = elementContents documentRoot

        elementName e @?= TaggedElementName (CodepageReference 0 6)
        elementAttributes e @?= []

        elementContents e @?= [ StrContent (Inline " X & Y")
                              , ElementContent (Element (TaggedElementName (CodepageReference 0 5))
                                                []
                                                [])
                              , StrContent (Inline " X")
                              , EntityContent (Entity 160)
                              , StrContent (Inline "=")
                              , EntityContent (Entity 160)
                              , StrContent (Inline "1 ")
                              ]
    where
      input = BS.pack [ 0x01, 0x01, 0x03, 0x00, 0x47, 0x46, 0x03, 0x20
                      , 0x58, 0x20, 0x26, 0x20, 0x59, 0x00, 0x05, 0x03
                      , 0x20, 0x58, 0x00, 0x02, 0x81, 0x20, 0x03, 0x3d
                      , 0x00, 0x02, 0x81, 0x20, 0x03, 0x31, 0x20, 0x00
                      , 0x01, 0x01
                      ]

testExample2 :: TestTree
testExample2 =
    testCase "Example 2" $
    case AP.parseOnly (wbxmlDocument <* AP.endOfInput) input of
      Left e -> assertFailure e
      Right Document{..} -> do
        let Header{..} = documentHeader

        headerVersion @?= Version 1 1
        headerPublicId @?= KnownPublicId 1
        headerCharset @?= Charset 106

        documentStrTable @?= StrTable "abc\^@ Enter name: \^@"

        elementName documentRoot @?= TaggedElementName (CodepageReference 0 7)
        elementAttributes documentRoot @?= []

        let [ ElementContent e ] = elementContents documentRoot

        elementName e @?= TaggedElementName (CodepageReference 0 5)
        elementAttributes e @?= [ Attribute (TaggedAttributeStart $ CodepageReference 0 9)
                                            [ StrAttributeValue (Reference 0) ]
                                , Attribute (TaggedAttributeStart  $ CodepageReference 0 5)
                                            []
                                ]

        let [ ElementContent e1, StrContent s, ElementContent e2 ] = elementContents e

        elementName e1 @?= TaggedElementName (CodepageReference 0 8)
        elementAttributes e1 @?= [ Attribute (TaggedAttributeStart $ CodepageReference 0 6)
                                             [ TaggedAttributeValue (CodepageReference 0 134) ]
                                 , Attribute (TaggedAttributeStart $ CodepageReference 0 8)
                                             [ StrAttributeValue (Inline "xyz")
                                             , TaggedAttributeValue (CodepageReference 0 133)
                                             , StrAttributeValue (Inline "/s")
                                             ]
                                 ]

        s @?= Reference 4

        elementName e2 @?= TaggedElementName (CodepageReference 0 6)
        elementAttributes e2 @?= [ Attribute (TaggedAttributeStart $ CodepageReference 0 7)
                                             []
                                 , Attribute (TaggedAttributeStart $ CodepageReference 0 10)
                                             [ StrAttributeValue (Inline "n") ]
                                 ]

    where
      input = BS.pack [ 0x01, 0x01, 0x6A, 0x12, 0x61, 0x62, 0x63, 0x00
                      , 0x20, 0x45, 0x6E, 0x74, 0x65, 0x72, 0x20, 0x6E
                      , 0x61, 0x6D, 0x65, 0x3A, 0x20, 0x00, 0x47, 0xC5
                      , 0x09, 0x83, 0x00, 0x05, 0x01, 0x88, 0x06, 0x86
                      , 0x08, 0x03, 0x78, 0x79, 0x7A, 0x00, 0x85, 0x03
                      , 0x2F, 0x73, 0x00, 0x01, 0x83, 0x04, 0x86, 0x07
                      , 0x0A, 0x03, 0x6E, 0x00, 0x01, 0x01, 0x01
                      ]

testParser :: TestTree
testParser = testGroup "Data.Wbxml.Parser"
             [ testExample1
             , testExample2
             ]
