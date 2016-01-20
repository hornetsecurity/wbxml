module Main (main) where

import           Test.Tasty

import           Test.Types

main :: IO ()
main = defaultMain $ testGroup "Data.Wbxml"
       [ testTypes
       ]
