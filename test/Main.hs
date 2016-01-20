module Main (main) where

import           Test.Tasty

import           Test.Parser
import           Test.Printer
import           Test.RoundtripBinary
import           Test.Types

main :: IO ()
main = defaultMain $ testGroup "Data.Wbxml"
       [ testTypes
       , testParser
       , testPrinter
       , testRoundtripBinary
       ]
