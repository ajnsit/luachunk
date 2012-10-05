-- Outputs a Lua chunk from the AST
-- Lua 5.1 only

module Scripting.Lua.Chunk.Writer (
  generateChunk
) where

import CustomPrelude

import qualified Data.ByteString as BS
import Data.ByteString.Lazy (toChunks)
import Data.ByteString.Char8 () -- Only import the IsString instance for ByteStrings
import Data.Bits (testBit, clearBit, setBit, (.|.))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)

import Scripting.Lua.Chunk.Types
import Scripting.Lua.Chunk.IEEE754 (encodeIEEEDouble)

generateChunk :: Chunk -> BS.ByteString
generateChunk c = BS.concat
  [ generateChunkHeader header
  , generateFunc header func
  ]
  where
    header = chunkHeader c
    func = chunkFunc c

generateChunkHeader :: Header -> BS.ByteString
generateChunkHeader h = BS.concat
  [ generateHeaderSignature
  , generateVersion (versionMajor h) (versionMinor h)
  , generateIntByte $ formatVersion h
  , generateBoolByte $ not $ bigEndian h
  , generateIntByte $ sizeOfInt h
  , generateIntByte $ sizeOfSizeT h
  , generateIntByte $ sizeOfInstruction h
  , generateIntByte $ sizeOfNumber h
  , generateBoolByte $ intMath h
  ]

generateFunc :: Header -> Func -> BS.ByteString
generateFunc h f = BS.concat
  [ generateFuncHeader   $ fHeader f
  , generateFuncInstrs   $ fInstrs f
  , generateFuncConsts h $ fConsts f
  , generateFuncNested h $ fNested f
  , generateFuncLines    $ fLines  f
  , generateFuncLocals   $ fLocals f
  , generateFuncUpvals   $ fUpvals f
  ]

generateFuncHeader :: FuncHeader -> BS.ByteString
generateFuncHeader f = BS.concat
  [ generateMaybeStr $ sourceName f
  , generateIntWord $ lineStart f
  , generateIntWord $ lineEnd f
  , generateIntByte $ numOfUpvalues f
  , generateIntByte $ numOfParams f
  , generateIsVararg (isVararg f) (isCompatVararg f) (needsArg f)
  , generateIntByte $ stackSize f
  ]

generateFuncInstrs :: [Instruction] -> BS.ByteString
generateFuncInstrs is = BS.concat
  ( generateIntWord (length is)
  : map generateInstr is
  )

generateInstr :: Instruction -> BS.ByteString
generateInstr i =
    case i of
      OP_MOVE a b       -> generateInstrAB    0 a b
      OP_LOADK a bx     -> generateInstrABx   1 a bx
      OP_LOADBOOL a b c -> generateInstrABC   2 a b c
      OP_LOADNIL a b    -> generateInstrAB    3 a b
      OP_GETUPVAL a b   -> generateInstrAB    4 a b
      OP_GETGLOBAL a bx -> generateInstrABx   5 a bx
      OP_GETTABLE a b c -> generateInstrABC   6 a b c
      OP_SETGLOBAL a bx -> generateInstrABx   7 a bx
      OP_SETUPVAL a b   -> generateInstrAB    8 a b
      OP_SETTABLE a b c -> generateInstrABC   9 a b c
      OP_NEWTABLE a b c -> generateInstrABC  10 a b c
      OP_SELF a b c     -> generateInstrABC  11 a b c
      OP_ADD a b c      -> generateInstrABC  12 a b c
      OP_SUB a b c      -> generateInstrABC  13 a b c
      OP_MUL a b c      -> generateInstrABC  14 a b c
      OP_DIV a b c      -> generateInstrABC  15 a b c
      OP_MOD a b c      -> generateInstrABC  16 a b c
      OP_POW a b c      -> generateInstrABC  17 a b c
      OP_UNM a b        -> generateInstrAB   18 a b
      OP_NOT a b        -> generateInstrAB   19 a b
      OP_LEN a b        -> generateInstrAB   20 a b
      OP_CONCAT a b c   -> generateInstrABC  21 a b c
      OP_JMP sbx        -> generateInstrsBx  22 sbx
      OP_EQ a b c       -> generateInstrABC  23 a b c
      OP_LT a b c       -> generateInstrABC  24 a b c
      OP_LE a b c       -> generateInstrABC  25 a b c
      OP_TEST a c       -> generateInstrAC   26 a c
      OP_TESTSET a b c  -> generateInstrABC  27 a b c
      OP_CALL a b c     -> generateInstrABC  28 a b c
      OP_TAILCALL a b c -> generateInstrABC  29 a b c
      OP_RETURN a b     -> generateInstrAB   30 a b
      OP_FORLOOP a sbx  -> generateInstrAsBx 31 a sbx
      OP_FORPREP a sbx  -> generateInstrAsBx 32 a sbx
      OP_TFORLOOP a c   -> generateInstrAC   33 a c
      OP_SETLIST a b c  -> generateInstrABC  34 a b c
      OP_CLOSE a        -> generateInstrA    35 a
      OP_CLOSURE a bx   -> generateInstrABx  36 a bx
      OP_VARARG a b     -> generateInstrAB   37 a b


generateHeaderSignature :: BS.ByteString
generateHeaderSignature = encodeStr "\ESCLua"

-- Assumes Little Endian format
generateVersion :: Int -> Int -> BS.ByteString
generateVersion major minor = concatbits 2 $ genbits 4 minor ++ genbits 4 major

generateIntByte :: Int -> BS.ByteString
generateIntByte = copybytes 1

generateIntWord :: Int -> BS.ByteString
generateIntWord = copybytes 4

generateBoolByte :: Bool -> BS.ByteString
generateBoolByte True = copybytes 1 1
generateBoolByte False = copybytes 1 0

generateIsVararg :: Bool -> Bool -> Bool -> BS.ByteString
generateIsVararg isVarargx isCompatVarargx needsArgx = BS.pack [i1 .|. i2 .|. i3]
  where
    i1 = if isVarargx then 1 else 0
    i2 = if isCompatVarargx then 2 else 0
    i3 = if needsArgx then 4 else 0

generateFuncConsts :: Header -> [Constant] -> BS.ByteString
generateFuncConsts h cs = BS.concat
  ( generateIntWord (length cs)
  : map (generateConst h) cs
  )

generateConst :: Header -> Constant -> BS.ByteString
generateConst _ CNil = generateIntByte 0
generateConst _ (CBool True) = BS.concat [generateIntByte 1, generateIntByte 1]
generateConst _ (CBool False) = BS.concat [generateIntByte 1, generateIntByte 0]
generateConst h (CNum n) = BS.concat [generateIntByte 3, BS.concat $ toChunks $ encodeIEEEDouble (not $ bigEndian h) n]
generateConst _ (CStr s) = BS.concat [generateIntByte 4, generateStr s]

generateFuncNested :: Header -> [Func] -> BS.ByteString
generateFuncNested h fs = BS.concat
  ( generateIntWord (length fs)
  : map (generateFunc h) fs
  )

generateFuncLines :: [Line] -> BS.ByteString
generateFuncLines ls = BS.concat
  ( generateIntWord (length ls)
  : map generateIntWord ls
  )

generateFuncLocals :: [Local] -> BS.ByteString
generateFuncLocals ls = BS.concat
  ( generateIntWord (length ls)
  : map generateLocal ls
  )

generateLocal :: Local -> BS.ByteString
generateLocal (Local s spc epc) = BS.concat
  [ generateStr s
  , generateIntWord spc
  , generateIntWord epc
  ]

generateFuncUpvals :: [Upval] -> BS.ByteString
generateFuncUpvals us = BS.concat
  ( generateIntWord (length us)
  : map generateStr us
  )

generateMaybeStr :: Maybe T.Text -> BS.ByteString
generateMaybeStr Nothing = generateIntWord 0
generateMaybeStr (Just s) = generateStr s

generateStr :: T.Text -> BS.ByteString
generateStr s = BS.concat [generateIntWord (1 + T.length s), encodeStr (T.append s "\0")]

-- Instruction Generators
generateInstrABC :: Int -> Int -> Int -> Int -> BS.ByteString
generateInstrABC op a b c = concatbits 4 $ genbits 6 op ++ genbits 8 a ++ genbits 9 c ++ genbits 9 b

generateInstrA :: Int -> Int -> BS.ByteString
generateInstrA op a = generateInstrABC op a 0 0

generateInstrAB :: Int -> Int -> Int -> BS.ByteString
generateInstrAB op a b = generateInstrABC op a b 0

generateInstrAC :: Int -> Int -> Int -> BS.ByteString
generateInstrAC op a = generateInstrABC op a 0

generateInstrABx :: Int -> Int -> Int -> BS.ByteString
generateInstrABx op a bx = concatbits 4 $ genbits 6 op ++ genbits 8 a ++ genbits 18 bx

-- Formula: Bx = sBx + MAX/2 where MAX = 2*31
generateInstrAsBx :: Int -> Int -> Int -> BS.ByteString
generateInstrAsBx op a sbx = concatbits 4 $ genbits 6 op ++ genbits 8 a ++ genbits 18 (sbx + 2147483648)
generateInstrsBx :: Int -> Int -> BS.ByteString
generateInstrsBx op sbx = concatbits 4 $ genbits 6 op ++ genbits 8 0 ++ genbits 18 (sbx + 2147483648)

genbits :: Int -> Int -> [Bool]
genbits count x = genbits' 0
  where
    genbits' bit
      | bit >= count = []
      | otherwise = testBit x bit : genbits' (bit+1)

-- Creates a word of specified number of BYTES out of the list of bit values (booleans)
concatbits :: Int -> [Bool] -> BS.ByteString
concatbits n = BS.pack . map (foldl bitset 0 . zip [0..]) . splitN 8 . take (8*n)
  where
    bitset :: Word8 -> (Int, Bool) -> Word8
    bitset x (i,True)  = setBit x i
    bitset x (i,False) = clearBit x i

-- Copies n bytes from an integer to a ByteString
copybytes :: Int -> Int -> BS.ByteString
copybytes n = concatbits n . genbits (n*8)

-- Split a string at regular intervals
splitN :: Int -> [a] -> [[a]]
splitN _ [] = []
splitN n xs = let (y1, y2) = splitAt n xs in y1 : splitN n y2

-- Convert Text to ByteString
encodeStr :: T.Text -> BS.ByteString
encodeStr = encodeUtf8

