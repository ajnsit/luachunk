-- Lua Chunk AST
-- Lua 5.1 Only

module Scripting.Lua.Chunk.Types
  ( Chunk(..)
  , Header(..)
  , Func(..)
  , FuncHeader(..)
  , Instruction(..)
  , FA, FB, FC, FBx, FsBx, FAx
  , Constant(..)
  , Line
  , Local(..)
  , Upval
  ) where

import CustomPrelude

import qualified Data.Text as T

-----------------------------------
-- D A T A   S T R U C T U R E S --
-----------------------------------

-- | A Parsed chunk
data Chunk = Chunk
  { chunkHeader :: Header -- ^ Parsed Header
  , chunkFunc   :: Func   -- ^ Parsed Main Function
  } deriving Show

-- | A Parsed Header
data Header = Header
  { versionMajor      :: Int  -- ^ Major version number (5)
  , versionMinor      :: Int  -- ^ Minor version number (1)
  , formatVersion     :: Int  -- ^ 0 = Official Format
  , bigEndian         :: Bool -- ^ Default : False (little endian)
  , sizeOfInt         :: Int  -- ^ In Bytes (4)
  , sizeOfSizeT       :: Int  -- ^ In Bytes (4)
  , sizeOfInstruction :: Int  -- ^ In Bytes (4)
  , sizeOfNumber      :: Int  -- ^ In Bytes (8)
  , intMath           :: Bool -- ^ Number Type is Integral. Default : False (Floating Point Math)
  } deriving Show

-- | A Parsed Function
data Func = Func
  { fHeader :: FuncHeader    -- ^ Header Data
  , fInstrs :: [Instruction] -- ^ List of instructions (code)
  , fConsts :: [Constant]    -- ^ List of constants
  , fNested :: [Func]        -- ^ List of function prototypes
  , fLines  :: [Line]        -- ^ Source line positions (optional debug data)
  , fLocals :: [Local]       -- ^ List of locals (optional debug data)
  , fUpvals :: [Upval]       -- ^ List of upvalues (optional debug data)
  }
  deriving Show

-- | A Parsed Function Header
data FuncHeader = FuncHeader
  { sourceName     :: Maybe T.Text -- ^ Name of SourceFile
  , lineStart      :: Int          -- ^ Line number where the function starts
  , lineEnd        :: Int          -- ^ Line number where the function ends
  , numOfUpvalues  :: Int          -- ^ Number of upvalues
  , numOfParams    :: Int          -- ^ Number of parameters
  , isVararg       :: Bool         -- ^ Whether this is vararg (Default : False)
  , isCompatVararg :: Bool         -- ^ Whether LUA_COMPAT_VARARG is defined
  , needsArg       :: Bool         -- ^ Whether it uses old style arguments i.e. arg instead of ... (Default :: False)
  , stackSize      :: Int          -- ^ Maximum number of registers used
  } deriving Show

-- | A Parsed VM Instruction
-- ** R(x) - register
-- ** Kst(x) - constant (in constant table)
-- ** RK(x) == if ISK(x) then Kst(INDEXK(x)) else R(x)
data Instruction
  = OP_MOVE FA FB        -- ^ R(A) := R(B)
  | OP_LOADK FA FBx      -- ^ R(A) := Kst(Bx)
  | OP_LOADBOOL FA FB FC -- ^ R(A) := (Bool)B; if (C) pc++
  | OP_LOADNIL FA FB     -- ^ R(A), R(A+1), ..., R(A+B) := nil
  | OP_GETUPVAL FA FB    -- ^ R(A) := UpValue[B]

  | OP_GETGLOBAL FA FBx  -- ^ R(A) := Gbl[Kst(Bx)]
  | OP_GETTABLE FA FB FC -- ^ R(A) := R(B)[RK(C)]

  | OP_SETGLOBAL FA FBx  -- Gbl[Kst(Bx)] := R(A)
  | OP_SETUPVAL FA FB    -- ^ UpValue[B] := R(A)
  | OP_SETTABLE FA FB FC -- ^ R(A)[RK(B)] := RK(C)

  | OP_NEWTABLE FA FB FC  -- ^ R(A) := {} (size = B,C)

  | OP_SELF FA FB FC  -- ^ R(A+1) := R(B); R(A) := R(B)[RK(C)]

  | OP_ADD FA FB FC -- ^ R(A) := RK(B) + RK(C)
  | OP_SUB FA FB FC -- ^ R(A) := RK(B) - RK(C)
  | OP_MUL FA FB FC -- ^ R(A) := RK(B) * RK(C)
  | OP_DIV FA FB FC -- ^ R(A) := RK(B) / RK(C)
  | OP_MOD FA FB FC -- ^ R(A) := RK(B) % RK(C)
  | OP_POW FA FB FC -- ^ R(A) := RK(B) ^ RK(C)
  | OP_UNM FA FB    -- ^ R(A) := -R(B)
  | OP_NOT FA FB    -- ^ R(A) := not R(B)
  | OP_LEN FA FB    -- ^ R(A) := length of R(B)

  | OP_CONCAT FA FB FC  -- ^ R(A) := R(B).. ... ..R(C)

  | OP_JMP FsBx  -- ^ pc+=sBx

  | OP_EQ FA FB FC  -- ^ if ((RK(B) == RK(C)) ~= A) then pc++
  | OP_LT FA FB FC  -- ^ if ((RK(B) <  RK(C)) ~= A) then pc++
  | OP_LE FA FB FC  -- ^ if ((RK(B) <= RK(C)) ~= A) then pc++

  | OP_TEST FA FC       -- ^ if not (R(A) <=> C) then pc++
  | OP_TESTSET FA FB FC -- ^ if (R(B) <=> C) then R(A) := R(B) else pc++

  | OP_CALL FA FB FC     -- ^ R(A), ... ,R(A+C-2) := R(A)(R(A+1), ... ,R(A+B-1))
  | OP_TAILCALL FA FB FC -- ^ return R(A)(R(A+1), ... ,R(A+B-1))
  | OP_RETURN FA FB      -- ^ return R(A), ... ,R(A+B-2)  (see note)

  | OP_FORLOOP FA FsBx -- ^ R(A)+=R(A+2); if R(A) <?= R(A+1) then { pc+=sBx; R(A+3)=R(A) }

  | OP_FORPREP FA FsBx  -- ^ R(A)-=R(A+2); pc+=sBx

  | OP_TFORLOOP FA FC  -- ^ R(A+3), ... ,R(A+2+C) := R(A)(R(A+1), R(A+2)); if R(A+3) ~= nil then R(A+2)=R(A+3) else pc++

  | OP_SETLIST FA FB FC  -- ^ R(A)[(C-1)*FPF+i] := R(A+i), 1 <= i <= B

  | OP_CLOSE FA       -- ^ close all variables in the stack up to (>=) R(A)
  | OP_CLOSURE FA FBx -- ^ R(A) := closure(KPROTO[Bx], R(A), ... ,R(A+n))

  | OP_VARARG FA FB  -- ^ R(A), R(A+1), ..., R(A+B-2) = vararg

  deriving Show


-- | Parsed opcode fields
type FA   = Int
-- | Parsed opcode fields
type FB   = Int
-- | Parsed opcode fields
type FC   = Int
-- | Parsed opcode fields
type FBx  = Int
-- | Parsed opcode fields
type FsBx = Int
-- | Parsed opcode fields
type FAx  = Int

-- | Parsed Constants
data Constant
  = CNil        -- ^ Nil Value
  | CBool Bool  -- ^ Boolean Value
  | CNum Double -- ^ Numeric Value
  | CStr T.Text -- ^ String Value

  deriving Show


-- DEBUG information present in chunks

-- | Line Numbers for each instruction
type Line = Int

-- | Local variables = Local <VarName> <StartPC> <EndPC>
-- where StartPC and EndPC denote the scope in which the local is active
data Local = Local T.Text Int Int deriving Show

-- | Upvalue names used in the function
type Upval = T.Text


