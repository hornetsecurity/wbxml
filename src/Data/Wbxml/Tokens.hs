module Data.Wbxml.Tokens where

import           Data.Word ( Word8 )

tokenSwitchPage :: Word8
tokenSwitchPage = 0x00

tokenEnd :: Word8
tokenEnd = 0x01

tokenEntity :: Word8
tokenEntity = 0x02

tokenStrI :: Word8
tokenStrI = 0x03

tokenLiteral :: Word8
tokenLiteral = 0x04

tokenPi :: Word8
tokenPi = 0x43

tokenStrT :: Word8
tokenStrT = 0x83

tokenOpaque :: Word8
tokenOpaque = 0xC3
