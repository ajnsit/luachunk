{-# LANGUAGE MagicHash #-}
{- Fast Parsing and writing IEEE Double values to/from ByteStrings -}
{- NOTE: This implementation is GHC specific -}
{- For portable (50-100x slower) versions, use data-binary-ieee754 -}
module Scripting.Lua.Chunk.IEEE754
  ( encodeIEEEDouble
  , decodeIEEEDouble
  ) where

import CustomPrelude

import qualified Data.ByteString.Lazy as BS
import Data.Binary.Put (runPut, putWord64be, putWord64le)
import Data.Binary.Get (runGet, getWord64be, getWord64le)

import GHC.Prim (unsafeCoerce#)
import GHC.Types (Double(D#))
import GHC.Word (Word64(W64#))

-- | Read a 64-bit double in supplied endianness format
decodeIEEEDouble :: Bool -> BS.ByteString -> Double
decodeIEEEDouble littleEndian = word64ToDouble . bytestringToWord64 littleEndian

-- | Write a 64-bit double to supplied endianness format
encodeIEEEDouble :: Bool -> Double -> BS.ByteString
encodeIEEEDouble littleEndian = word64ToBytestring littleEndian . doubleToWord64

-- Conversion to and from IEE754 representation for numbers
-- Assumes that Double values are stored in IEEE 754 format (always true for GHC)
--  and that the endianness of the word matches that of the system
word64ToDouble :: Word64 -> Double
word64ToDouble (W64# x) = D# (unsafeCoerce# x)
doubleToWord64 :: Double -> Word64
doubleToWord64 (D# x) = W64# (unsafeCoerce# x)

-- Bytestring <-> Word64 conversion functions
bytestringToWord64 :: Bool -> BS.ByteString -> Word64
bytestringToWord64 littleEndian = runGet $ if littleEndian then getWord64le else getWord64be
word64ToBytestring :: Bool -> Word64 -> BS.ByteString
word64ToBytestring littleEndian = runPut . if littleEndian then putWord64le else putWord64be


