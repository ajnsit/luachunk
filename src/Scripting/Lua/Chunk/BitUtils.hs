-- Useful Bit manipulation functions
module Scripting.Lua.Chunk.BitUtils (
  bitRange,
  bitSet
) where

import CustomPrelude

import Data.Bits (testBit, clearBit, setBit)

-- | Get a range of bits from an Int
bitRange :: Int -> Int -> Int -> Int
bitRange from to x = bitRange' from to 0 0
  where
    bitRange' f t bit result
      | f >= t = result
      | otherwise = bitRange' (f+1) t (bit+1) (bitSet (testBit x f) result bit)

-- | Set a bit in an integer
bitSet :: Bool -> Int -> Int -> Int
bitSet True x i = setBit x i
bitSet False x i = clearBit x i


