-- Decoder for Lua Chunks
-- Lua 5.1 Only

module Scripting.Lua.Chunk.Reader (
  parseChunk
) where

import CustomPrelude

import qualified Data.ByteString as BS
import Data.ByteString.Lazy (fromChunks)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Data.Bits (testBit)
import qualified Data.Attoparsec as A

import Scripting.Lua.Chunk.Types
import Scripting.Lua.Chunk.Endianness
import Scripting.Lua.Chunk.IEEE754
import Scripting.Lua.Chunk.BitUtils


---------------------------------
-- P A R S I N G   C H U N K S --
---------------------------------

-- | Parse a Bytestring into a Chunk
parseChunk :: BS.ByteString -> Chunk
parseChunk bs = case A.parse chunk bs of
  A.Done _ x -> x
  A.Fail _ _ e -> error e
  A.Partial _ -> error "Incomplete input"

-- Parse a chunk
chunk :: A.Parser Chunk
chunk = do
  h <- header
  f <- func h
  return $ Chunk h f

-- Parse and validate the header
-- TODO: Make the header configuration flexible
header :: A.Parser Header
header = do
  -- Header signature
  sig <- A.take 4
  unless (sig == encodeStr "\ESCLua") $ error "Invalid header signature"
  -- Chunk Version
  ver <- A.anyWord8
  unless (ver == 0x51) $ error "Invalid version. Please use Luac version 5.1"
  let majorVer = 5
  let minorVer = 1
  -- Format Version
  fver <- A.anyWord8
  unless (fver == 0) $ error "Invalid format version. Please use the official version"
  -- Endianness
  endianness <- A.anyWord8
  let be = endianness == 0
  -- Size of Int
  intSize <- A.anyWord8
  unless (intSize == 4) $ error "Invalid size of Int. Please make sure you are on a 32 bit machine"
  -- Size of size_t
  sizeTSize <- A.anyWord8
  unless (sizeTSize == 4) $ error "Invalid size of size_t. Please make sure you are on a 32 bit machine"
  -- Size of Instruction
  instrSize <- A.anyWord8
  unless (instrSize == 4) $ error "Invalid size of Instruction. Standard Lua instructions are 4 Bytes."
  -- Size of Number
  numSize <- A.anyWord8
  unless (numSize == 8) $ error "Invalid size of Number. A number must be 8 bytes (Word64 in length)"
  -- Integral
  integral <- A.anyWord8
  unless (integral == 0) $ error "Floating point math must be supported"
  let im = False
  return $ Header
    majorVer
    minorVer
    (fromIntegral fver)
    be
    (fromIntegral intSize)
    (fromIntegral sizeTSize)
    (fromIntegral instrSize)
    (fromIntegral numSize)
    im


-- Parse a Function
func :: Header -> A.Parser Func
func h = do
  fh   <- funcHeader h
  li   <- listInstr h
  cs   <- listConst h
  fs   <- listFunc h
  ls   <- listLine h
  locs <- listLocal h
  us   <- listUpval h
  return $ Func fh li cs fs ls locs us

-- Parse a function header
funcHeader :: Header -> A.Parser FuncHeader
funcHeader h = do
  -- Source name
  sname <- str h
  -- Start Line
  sl <- intP $ sizeOfInt h
  -- End line
  el <- intP $ sizeOfInt h
  -- Number of up values
  nupx <- A.anyWord8
  let nup = fromIntegral nupx
  -- Number of params
  npx <- A.anyWord8
  let nparams = fromIntegral npx
  -- Vararg flags
  varx <- A.anyWord8
  let isVarargx = testBit varx 0
  let isCompatVarargx = testBit varx 1
  let needsArgx = testBit varx 2
  -- Maximum stack size
  maxx <- A.anyWord8
  let maxss = fromIntegral maxx
  return $ FuncHeader
    sname
    sl
    el
    nup
    nparams
    isVarargx
    isCompatVarargx
    needsArgx
    maxss

-- Parse a list of instructions
listInstr :: Header -> A.Parser [Instruction]
listInstr h = do
  n <- intP $ sizeOfInt h
  replicateM n instr


-- Parse an instruction
instr :: A.Parser Instruction
instr = do
  -- Instructions are always 4 bytes in standard Lua
  x <- intP 4
  let op = bitRange 0 6 x
  let a  = bitRange 6 14 x
  let c  = bitRange 14 23 x
  let b  = bitRange 23 32 x
  let bx = bitRange 14 32 x
  -- Formula: sBx = Bx - MAX/2 where MAX = 2*31 = 2147483648
  -- HOWEVER, Looking at the Lua sources, we realise that the number to use is 2^17-1 = 131071
  let sbx = bx - 131071
  return $
    case op of
      0  -> OP_MOVE a b
      1  -> OP_LOADK a bx
      2  -> OP_LOADBOOL a b c
      3  -> OP_LOADNIL a b
      4  -> OP_GETUPVAL a b
      5  -> OP_GETGLOBAL a bx
      6  -> OP_GETTABLE a b c
      7  -> OP_SETGLOBAL a bx
      8  -> OP_SETUPVAL a b
      9  -> OP_SETTABLE a b c
      10 -> OP_NEWTABLE a b c
      11 -> OP_SELF a b c
      12 -> OP_ADD a b c
      13 -> OP_SUB a b c
      14 -> OP_MUL a b c
      15 -> OP_DIV a b c
      16 -> OP_MOD a b c
      17 -> OP_POW a b c
      18 -> OP_UNM a b
      19 -> OP_NOT a b
      20 -> OP_LEN a b
      21 -> OP_CONCAT a b c
      22 -> OP_JMP sbx
      23 -> OP_EQ a b c
      24 -> OP_LT a b c
      25 -> OP_LE a b c
      26 -> OP_TEST a c
      27 -> OP_TESTSET a b c
      28 -> OP_CALL a b c
      29 -> OP_TAILCALL a b c
      30 -> OP_RETURN a b
      31 -> OP_FORLOOP a sbx
      32 -> OP_FORPREP a sbx
      33 -> OP_TFORLOOP a c
      34 -> OP_SETLIST a b c
      35 -> OP_CLOSE a
      36 -> OP_CLOSURE a bx
      37 -> OP_VARARG a b
      _  -> error "Unrecognised opcode"


-- List of constants
listConst :: Header -> A.Parser [Constant]
listConst h = do
  n <- intP $ sizeOfInt h
  replicateM n (constant h)

-- A Constant
constant :: Header -> A.Parser Constant
constant h = do
  typ <- A.anyWord8
  case typ of
    0 -> return CNil
    1 -> liftM CBool boolean
    -- NOTE: Constant type 2 is not used
    3 -> liftM CNum $ number h
    4 -> do
      s <- str h
      case s of
        Nothing -> error "Null string encountered in constants section"
        Just si -> return $ CStr si
    _ -> error $ "Unrecognised constant type: " ++ T.unpack (show typ)


-- List of Functions
listFunc :: Header -> A.Parser [Func]
listFunc h = do
  n <- intP $ sizeOfInt h
  replicateM n (func h)

-- List of Lines
listLine :: Header -> A.Parser [Line]
listLine h = do
  n <- intP $ sizeOfInt h
  replicateM n (intP 4)

-- List of Locals
listLocal :: Header -> A.Parser [Local]
listLocal h = do
  n <- intP $ sizeOfInt h
  replicateM n (local h)

-- Parsed Local
local :: Header -> A.Parser Local
local h = do
  s <- str h
  case s of
    Nothing -> error "Unnamed local variable encountered!"
    Just si -> do
      startpc <- intP $ sizeOfInt h
      endpc <- intP $ sizeOfInt h
      return $ Local si startpc endpc

-- List of Upvals
listUpval :: Header -> A.Parser [Upval]
listUpval h = do
  n <- intP $ sizeOfInt h
  replicateM n ss
  where
    ss = do
      s <- str h
      return $ fromMaybe (error "Unnamed upval encountered!") s


-- Parse a Boolean
boolean :: A.Parser Bool
boolean = do
  b <- A.anyWord8
  return $
    case b of
      0 -> False
      _ -> True

number :: Header -> A.Parser Double
number h = do
  w64bs <- A.take $ sizeOfNumber h
  return $ decodeIEEEDouble (not $ bigEndian h) $ fromChunks [w64bs]

-- Parse a String
str :: Header -> A.Parser (Maybe T.Text)
str h = do
  n <- intP $ sizeOfSizeT h
  -- when (n /= 11) $ error $ show n
  if n<=0
    then return Nothing
    else do
      bs <- A.take (n-1)
      -- Trailing null
      A.word8 0
      return $ Just $ decodeStr bs


-- Parse an Integer
-- The endianness is taken from the system
intP :: Int -> A.Parser Int
intP n = do
    bs <- replicateM n A.anyWord8
    let bsn = map fromIntegral $ trans bs
    return $ foldr (\b p -> p*256 + b) 0 bsn
      where
        trans = if isBigEndian then reverse else id

-- Convert Text to ByteString
encodeStr :: T.Text -> BS.ByteString
encodeStr = encodeUtf8
decodeStr :: BS.ByteString -> T.Text
decodeStr = decodeUtf8

