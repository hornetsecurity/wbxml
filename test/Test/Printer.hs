{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Test.Printer ( testPrinter ) where

import qualified Data.ByteString.Builder as B

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Wbxml.Printer
import           Data.Wbxml.Types

testExample1 :: TestTree
testExample1 = testCase "Example 1" $
               B.toLazyByteString (wbxmlDocument doc) @?= "\0\1\106\0\68\3Foo\0\1"
    where
      doc = Document{..}
      documentHeader = Header (Version 1 0) (KnownPublicId 1) (Charset 106)
      documentStrTable = StrTable ""
      documentRoot = Element{..}
      elementName = TaggedElementName (CodepageReference 0 4)
      elementAttributes = []
      elementContents = [ StrContent (Inline "Foo") ]

testPrinter :: TestTree
testPrinter = testGroup "Data.Wbxml.Printer"
              [ testExample1
              ]

