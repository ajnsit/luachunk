{- Detect Endianness on the system -}
{- This implementation is highly GHC specific -}
module Scripting.Lua.Chunk.Endianness (
  isLittleEndian,
  isBigEndian
) where

import CustomPrelude

import Foreign.Marshal.Alloc (alloca)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (peek, poke)
import System.IO.Unsafe (unsafePerformIO)

endianCheck :: Word8
endianCheck = unsafePerformIO $ alloca $ \p -> poke p (0x01020304::Word32) >> peek (castPtr p::Ptr Word8)

isLittleEndian :: Bool
isLittleEndian = endianCheck == 4

isBigEndian :: Bool
isBigEndian = endianCheck == 1

