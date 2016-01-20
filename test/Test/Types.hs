{-# LANGUAGE OverloadedStrings #-}

module Test.Types ( testTypes ) where

import           Test.Tasty
import           Test.Tasty.HUnit

import           Data.Wbxml.Types

testLookupStr :: TestTree
testLookupStr =
    testCase "lookupStr" $ do
      0 `lookupStr` StrTable "" @?= Nothing
      0 `lookupStr` StrTable "\^@" @?= Just ""

      let table = StrTable "Lorem\^@Ipsum\^@Dolor\^@"
      0 `lookupStr` table @?= Just "Lorem"
      6 `lookupStr` table @?= Just "Ipsum"
      12 `lookupStr` table @?= Just "Dolor"

      17 `lookupStr` table @?= Just ""
      18 `lookupStr` table @?= Nothing

      2 `lookupStr` table @?= Just "rem"

testTypes :: TestTree
testTypes = testGroup "Data.Wbxml.Types"
             [ testLookupStr
             ]
