{-# LANGUAGE OverloadedStrings     #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.RoundtripBinary ( testRoundtripBinary ) where

import           Control.Applicative        ( empty )
import           Control.Monad.Trans.State  ( evalState, runStateT )
import           Control.Monad.Trans.Writer ( execWriterT )

import           Data.Bits                  ( (.&.) )
import           Data.Word                  ( Word32, Word8 )

import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Builder    as B
import qualified Data.ByteString.Lazy       as LBS

import           Test.SmallCheck.Series
import           Test.Tasty
import           Test.Tasty.SmallCheck

import qualified Data.Wbxml.Parser          as Parser
import qualified Data.Wbxml.Printer         as Printer
import           Data.Wbxml.Types

suchThat :: Series m a -> (a -> Bool) -> Series m a
suchThat s p = s >>= \x -> if p x then pure x else empty

elements :: Monad m => [a] -> Series m a
elements [] = empty
elements (x : xs) = cons0 x \/ decDepth (elements xs)

listSeries :: Serial m a => Series m [a]
listSeries = cons0 [] \/ localDepth (`div` 2) ((:) <$> series <~> listSeries)

isGlobalToken :: Word8 -> Bool
isGlobalToken t = (t .&. 0x3f) <= 4

tagTokens :: Monad m => Series m CodepageReference
tagTokens = CodepageReference <$> elements [ 0, 1 ]
                              <~> elements (filter (not . isGlobalToken)
                                                   [0 .. 255])

attrStartTokens :: Monad m => Series m CodepageReference
attrStartTokens = CodepageReference <$> elements [ 0, 1 ]
                                    <~> elements (filter (not . isGlobalToken)
                                                         [0 .. 127])

attrValueTokens :: Monad m => Series m CodepageReference
attrValueTokens = CodepageReference <$> elements [ 0, 1 ]
                                    <~> elements (filter (not . isGlobalToken)
                                                         [128 .. 255])

instance Monad m => Serial m Word8 where
    series = cons0 minBound \/ decDepth (succ <$> series)

instance Monad m => Serial m Word32 where
    series = cons0 minBound \/ decDepth (succ <$> series)

instance Monad m => Serial m BS.ByteString where
    series = BS.pack . map getPositive <$> series

instance Monad m => Serial m StrTable where
    series = newtypeCons (StrTable . BS.concat . map (BS.append "\^@"))

instance Monad m => Serial m CodepageReference where
    series = newtypeCons (CodepageReference 0)

instance Monad m => Serial m Str where
    series = cons1 Inline \/ cons1 Reference

instance Monad m => Serial m Version where
    series = do
        major <- series `suchThat` (\x -> x >= 1 && x < 5)
        minor <- series `suchThat` (\x -> x >= 0 && x < 4)
        return $ Version major minor

instance Monad m => Serial m PublicId where
    series = cons1 (KnownPublicId . getPositive)
        \/ cons1 LiteralPublicId

instance Monad m => Serial m Charset where
    series = newtypeCons Charset

instance Monad m => Serial m Entity where
    series = newtypeCons Entity

instance Monad m => Serial m Extension where
    series = do
        eid <- elements [0 .. 2]
        value <- series
        return $ Extension eid value

instance Monad m => Serial m ElementName where
    series = (TaggedElementName <$> tagTokens)
        \/ cons1 LiteralElementName

instance Monad m => Serial m AttributeStart where
    series = (TaggedAttributeStart <$> attrStartTokens)
        \/ cons1 LiteralAttributeStart

instance Monad m => Serial m AttributeValue where
    series = (TaggedAttributeValue <$> attrValueTokens)
        \/ cons1 StrAttributeValue
        \/ cons1 ExtensionAttributeValue
        \/ cons1 EntityAttributeValue

instance Monad m => Serial m Attribute where
    series = decDepth $ Attribute <$> series <~> listSeries

instance Monad m => Serial m Element where
    series = decDepth $ Element <$> series <~> listSeries <~> listSeries

instance Monad m => Serial m ProcessingInstruction where
    series = cons2 ProcessingInstruction

instance Monad m => Serial m Content where
    series = cons1 ElementContent
        \/ cons1 StrContent
        \/ cons1 ExtensionContent
        \/ cons1 EntityContent
        \/ cons1 ProcessingInstructionContent
        \/ cons1 OpaqueContent

instance Monad m => Serial m Header where
    series = cons3 Header

instance Monad m => Serial m Document where
    series = cons3 Document

propRoundtripBinary :: (Eq a, Show a)
                    => (a -> Printer.Printer ())
                    -> Parser.Parser a
                    -> a
                    -> Bool
propRoundtripBinary printer parser o =
    case result of
        Right decoded
            | decoded == o -> True
            | otherwise -> error ("object: " ++ show o
                                  ++ " modified during roundtrip: " ++ show decoded)
        Left e -> error ("object: " ++ show o
                         ++ " failed to decode: " ++ e
                         ++ " binary: " ++ show binary)
  where
    result = fst <$> AP.parseOnly (runStateT parser Parser.initialParserState <*
                                       AP.endOfInput)
                                  binary
    binary = LBS.toStrict $
        B.toLazyByteString $
            evalState (execWriterT $ printer o) Printer.initialPrinterState

testRoundtripBinary :: TestTree
testRoundtripBinary =
    testGroup "Data.Wbxml.Types (RoundtripBinary)"
              [ testProperty "Str" (propRoundtripBinary Printer.str Parser.str)
              , testProperty "Version" (propRoundtripBinary Printer.version Parser.version)
              , testProperty "PublicId" (propRoundtripBinary Printer.publicId Parser.publicId)
              , testProperty "Charset" (propRoundtripBinary Printer.charset Parser.charset)
              , testProperty "Entity" (propRoundtripBinary Printer.entity Parser.entity)
              , testProperty "Extension" (propRoundtripBinary Printer.extension Parser.extension)
              , testProperty "AttributeStart" (propRoundtripBinary Printer.attributeStart Parser.attributeStart)
              , testProperty "AttributeValue" (propRoundtripBinary Printer.attributeValue Parser.attributeValue)
              , testProperty "Attribute" (propRoundtripBinary Printer.attribute Parser.attribute)
              , testProperty "Element" (propRoundtripBinary Printer.element Parser.element)
              , testProperty "ProcessingInstruction" (propRoundtripBinary Printer.processingInstruction Parser.processingInstruction)
              , testProperty "Content" (propRoundtripBinary Printer.content Parser.content)
              , testProperty "Header" (propRoundtripBinary Printer.header Parser.header)
              , testProperty "StrTable" (propRoundtripBinary Printer.strTable Parser.strTable)
              , testProperty "Document" (propRoundtripBinary Printer.document Parser.document)
              ]
