{-# LANGUAGE CPP #-}
module Graphics.Text.TrueType.Bytecode where

import Prelude hiding ( EQ, GT, LT )
import Control.Applicative( (<$>) )
{-import Control.Monad.State.Strict( modify )-}
import Data.Bits( (.&.), testBit )
import Data.Binary.Get( Get, getWord8, getWord16be )
import Data.Monoid( mempty )
import Data.Int( Int32 )
import qualified Data.Map as M
import Data.Word( Word8, Word16 )
import qualified Data.Vector.Unboxed as VU

import Graphics.Text.TrueType.Types

type InstructionFlag = Bool

data Instruction
    -- | Pop one argument from the stack
    = AA 
    | ABS
    | ADD
    | ALIGNPTS
    | ALIGNRP
    | AND
    | CALL
    | CEILING
    | CINDEX
    | CLEAR
    | DEBUG
    | DELTAC1
    | DELTAC2
    | DELTAC3
    | DELTAP1
    | DELTAP2
    | DELTAP3
    | DEPTH
    | DIV
    | DUP
    | EIF
    | ELSE
    | ENDF
    | EQ
    | EVEN
    | FDEF
    | FLIPOFF
    | FLIPON
    | FLIPPT
    | FLIPRGOFF
    | FLIPRGON
    | FLOOR
    | GC PositionSource
    | GETINFO
      -- | Get Freedom vector
    | GFV
    | GPV
    | GT
    | GTEQ
    | IDEF
    | IF
    | INSTCTRL
    | IP
    | ISECT
    | IUP Direction
    | JMPR
    | JROF
    | JROT
    | LOOPCALL
    | LT
    | LTEQ
    | MAX
    | MD PositionSource
    | MDAP Rounding
    | MDRP PointReset DistanceKeeping Rounding Word8
    | MIAP Rounding
    | MIN
    | MINDEX
    | MIRP PointReset DistanceKeeping Rounding Word8
    | MPPEM
    | MPS
    | MSIRP PointReset
    | MUL
    | NEG
    | NEQ
    | NOT
    | NPUSHB (VU.Vector Word8)
    | NPUSHW (VU.Vector Word16)
    | NROUND Word8
    | ODD
    | OR
    | POP
    | PUSHB (VU.Vector Word8)
    | PUSHW (VU.Vector Word16)
    | RCVT
    | RDTG
    | ROFF
    | ROLL
    | ROUND Word8
    | RS
    | RTDG
    | RTG
    | RTHG
    | RUTG
    | S45ROUND
    | SANGW
    | SCANCTRL
    | SCANTYPE
    | SCFS
    | SCVTCI
    | SDB
    | SDPVTL Parallelism
    | SDS
    | SFVFS
    | SFVTCA Direction
    | SFVTL Parallelism
    | SFVTPV
    | SHC CountourRef
    | SHP CountourRef
    | SHPIX
    | SHZ CountourRef
    | SLOOP
    | SMD
    | SPVFS
    | SPVTCA Direction
    | SPVTL Parallelism
    | SROUND
    | SRP0
    | SRP1
    | SRP2
    | SSW
    | SSWCI
    | SUB
    | SVTCA Direction
    | SWAP
    | SZP0
    | SZP1
    | SZP2
    | SZPS
    | UTP
    | WCVTF
    | WCVTP
    | WS
    deriving (Eq, Show)

data CountourRef
    = UseRP2InZP1
    | UseRP1InZP0
    deriving (Eq, Show)

data Parallelism
    = VectorParallel
    | VectorPerpendicular
    deriving (Eq, Show)

data PositionSource
    = PositionCurrent
    | PositionOriginal
    deriving (Eq, Show)

data Direction
    = DirectionX
    | DirectionY
    deriving (Eq, Show)

data Rounding
    = RoundingNone
    | Rounding
    deriving (Eq, Show)

data PointReset
    = ResetNot
    | ResetRP0
    deriving (Eq, Show)

data DistanceKeeping
    = DistanceFree
    | DistanceGreaterThanMin
    deriving (Eq, Show)

data ByteCodeProgram
    = If     [ByteCodeProgram]
    | IfElse [ByteCodeProgram] [ByteCodeProgram]
    | Function [ByteCodeProgram]
    | Sequence [Instruction]
    deriving (Eq, Show)

instructionToProgram :: [Instruction]-> [ByteCodeProgram]
instructionToProgram = firstOfThird . go []
  where
    firstOfThird (f, _, _) = f

    go acc [] = (reverse acc, [], [])
    go acc (IF : rest) =
      case go [] rest of
        (ifBranch, [], final) -> go (If ifBranch : acc) final
        (ifBranch, elseBranch, final) ->
            go (IfElse ifBranch elseBranch : acc) final
    go acc (EIF : rest) = (reverse acc, [], rest)
    go acc (ELSE : rest) = (acc, elseBranch, final)
      where (elseBranch, _, final) = go [] rest
    go acc (FDEF : rest) = go (Function def: acc) final
      where (def, _, final) = go [] rest
    go acc (ENDF : rest) = (reverse acc, [], rest)
    go acc (x : xs) = go (Sequence [x] : acc) xs


inRange :: Ord a => a -> (a, a) -> Bool
inRange v (lo, hi) = lo <= v && v <= hi

getInstr :: Get Instruction 
getInstr = getWord8 >>= go
  where
    mirpParse x constructor = constructor reset distance rounding distanceType
      where
        reset | x `testBit` 4 = ResetRP0
              | otherwise = ResetNot

        distance | x `testBit` 3 = DistanceGreaterThanMin
                 | otherwise = DistanceFree

        rounding | x `testBit` 2 = Rounding
                 | otherwise = RoundingNone

        distanceType = x .&. 0x3

    go w = case w of
      0x7F -> return AA
      0x64 -> return ABS
      0x60 -> return ADD
      0x27 -> return ALIGNPTS
      0x3C -> return ALIGNRP
      0x5A -> return AND
      0x2B -> return CALL
      0x67 -> return CEILING
      0x25 -> return CINDEX
      0x22 -> return CLEAR
      0x4F -> return DEBUG
      0x73 -> return DELTAC1
      0x74 -> return DELTAC2
      0x75 -> return DELTAC3
      0x5D -> return DELTAP1
      0x71 -> return DELTAP2
      0x72 -> return DELTAP3
      0x24 -> return DEPTH
      0x62 -> return DIV
      0x20 -> return DUP
      0x59 -> return EIF
      0x1B -> return ELSE
      0x2D -> return ENDF
      0x54 -> return EQ
      0x57 -> return EVEN
      0x2C -> return FDEF
      0x4E -> return FLIPOFF
      0x4D -> return FLIPON
      0x80 -> return FLIPPT
      0x82 -> return FLIPRGOFF
      0x81 -> return FLIPRGON
      0x66 -> return FLOOR
      0x46 -> return $ GC PositionCurrent
      0x47 -> return $ GC PositionOriginal
      0x88 -> return GETINFO
      0x0D -> return GFV
      0x0C -> return GPV
      0x52 -> return GT
      0x53 -> return GTEQ
      0x89 -> return IDEF
      0x58 -> return IF
      0x8E -> return INSTCTRL
      0x39 -> return IP
      0x0F -> return ISECT
      0x30 -> return $ IUP DirectionY
      0x31 -> return $ IUP DirectionX
      0x1C -> return JMPR
      0x79 -> return JROF
      0x78 -> return JROT
      0x2A -> return LOOPCALL
      0x50 -> return LT
      0x51 -> return LTEQ
      0x8B -> return MAX
      0x49 -> return $ MD PositionCurrent
      0x4A -> return $ MD PositionOriginal
      0x2E -> return $ MDAP RoundingNone
      0x2F -> return $ MDAP Rounding
      x | x `inRange` (0xC0, 0xDF) -> return $ mirpParse x MDRP
      0x3E -> return $ MIAP RoundingNone
      0x3F -> return $ MIAP Rounding
      0x8C -> return MIN
      0x26 -> return MINDEX
      x | x `inRange` (0xE0, 0xFF) -> return $ mirpParse x MIRP
      0x4B -> return MPPEM
      0x4C -> return MPS
      0x3A -> return $ MSIRP ResetNot
      0x3B -> return $ MSIRP ResetRP0
      0x63 -> return MUL
      0x65 -> return NEG
      0x55 -> return NEQ
      0x5C -> return NOT
      0x40 -> do
        count <- fromIntegral <$> getWord8
        NPUSHB <$> VU.replicateM count getWord8

      0x41 -> do
        count <- fromIntegral <$> getWord8
        NPUSHW <$> VU.replicateM count getWord16be
      x | x `inRange` (0x6C, 0x6F) ->
          return . NROUND $ x .&. 3
      0x56 -> return ODD
      0x5B -> return OR
      0x21 -> return POP
      x | x `inRange` (0xB0, 0xB7) -> do
          let count = fromIntegral $ (x .&. 0x7) + 1
          PUSHB <$> VU.replicateM count getWord8
      x | x `inRange` (0xB8, 0xBF) -> do
          let count = fromIntegral $ (x .&. 0x7) + 1
          PUSHW <$> VU.replicateM count getWord16be
      0x45 -> return RCVT
      0x7D -> return RDTG
      0x7A -> return ROFF
      0x8A -> return ROLL
      x | x `inRange` (0x68, 0x6B) ->
          return $ ROUND (x .&. 0x3)
      0x43 -> return RS
      0x3D -> return RTDG
      0x18 -> return RTG
      0x19 -> return RTHG
      0x7C -> return RUTG
      0x77 -> return S45ROUND
      0x7E -> return SANGW
      0x85 -> return SCANCTRL
      0x8D -> return SCANTYPE
      0x48 -> return SCFS
      0x1D -> return SCVTCI
      0x5E -> return SDB
      0x86 -> return $ SDPVTL VectorParallel
      0x87 -> return $ SDPVTL VectorPerpendicular
      0x5F -> return SDS
      0x0B -> return SFVFS
      0x04 -> return $ SFVTCA DirectionY
      0x05 -> return $ SFVTCA DirectionX
      0x08 -> return $ SFVTL VectorParallel
      0x09 -> return $ SFVTL VectorPerpendicular
      0x0E -> return SFVTPV
      0x34 -> return $ SHC UseRP2InZP1
      0x35 -> return $ SHC UseRP1InZP0
      0x32 -> return $ SHP UseRP2InZP1
      0x33 -> return $ SHP UseRP1InZP0
      0x38 -> return SHPIX
      0x36 -> return $ SHZ UseRP2InZP1
      0x37 -> return $ SHZ UseRP1InZP0
      0x17 -> return SLOOP
      0x1A -> return SMD
      0x0A -> return SPVFS
      0x02 -> return $ SPVTCA DirectionY
      0x03 -> return $ SPVTCA DirectionX
      0x06 -> return $ SPVTL VectorParallel
      0x07 -> return $ SPVTL VectorPerpendicular
      0x76 -> return SROUND
      0x10 -> return SRP0
      0x11 -> return SRP1
      0x12 -> return SRP2
      0x1F -> return SSW
      0x1E -> return SSWCI
      0x61 -> return SUB
      0x00 -> return $ SVTCA DirectionY
      0x01 -> return $ SVTCA DirectionX
      0x23 -> return SWAP
      0x13 -> return SZP0
      0x14 -> return SZP1
      0x15 -> return SZP2
      0x16 -> return SZPS
      0x29 -> return UTP
      0x70 -> return WCVTF
      0x44 -> return WCVTP
      0x42 -> return WS
      _ -> fail "instruction reader coverage pleaser"

type Point = (F26Dot6, F26Dot6)

data GraphicalState = GraphicalState
    { _stAutoFlip           :: !Bool
    , _stCutIn              :: {-# UNPACK #-} !F26Dot6
    , _stDeltaBase          :: {-# UNPACK #-} !Int32
    , _stDeltaShift         :: {-# UNPACK #-} !Int32
    , _stDualProjection     :: !(Maybe Point)
    , _stFreedomVector      :: !Point
    , _stInstructionControl :: !Bool
    , _stLoop               :: {-# UNPACK #-} !Int32
    , _stMinDistance        :: {-# UNPACK #-} !F26Dot6
    , _stProjectionVector   :: !Point
    , _stRoundState         :: {-# UNPACK #-} !Int32
    , _stRp0                :: {-# UNPACK #-} !Int32
    , _stRp1                :: {-# UNPACK #-} !Int32
    , _stRp2                :: {-# UNPACK #-} !Int32
    , _stScanControl        :: !Bool
    , _stSingleWidthCutIn   :: {-# UNPACK #-} !F26Dot6
    , _stSingleWidthValue   :: {-# UNPACK #-} !F26Dot6
    , _stZp0                :: {-# UNPACK #-} !Int32
    , _stZp1                :: {-# UNPACK #-} !Int32
    , _stZp2                :: {-# UNPACK #-} !Int32
    , _stFunctions          :: !(M.Map Int32 [ByteCodeProgram])
    }
    deriving (Eq, Show)

initialState :: GraphicalState
initialState = GraphicalState
    { _stAutoFlip  = True
    , _stCutIn     = 17 / 16
    , _stDeltaBase = 9
    , _stDeltaShift = 3
    , _stDualProjection = Nothing
    , _stFreedomVector = (1, 0)
    , _stInstructionControl = False
    , _stLoop = 1
    , _stMinDistance = 1
    , _stProjectionVector = (1, 0)
    , _stRoundState = 1
    , _stRp0 = 0
    , _stRp1 = 0
    , _stRp2 = 0
    , _stScanControl = False
    , _stSingleWidthCutIn = 0
    , _stSingleWidthValue = 0
    , _stZp0 = 1
    , _stZp1 = 1
    , _stZp2 = 1
    , _stFunctions = mempty
    }

#if 0

evaluate :: [ByteCodeProgram] -> ()
evaluate instrs =
  where
    byteExec []                        stack = stack
    byteExec (If        _ : instr) (0:stack) = byteExec instr stack
    byteExec (If thenBody : instr) (_:stack) = byteExec (thenBody ++ instr) stack
    byteExec (IfElse _ elseBody)   (0:stack) = byteExec (elseBody ++ instr) stack
    byteExec (IfElse thenBody _)   (_:stack) = byteExec (thenBody ++ instr) stack
    byteExec (Function functionBody : instr) (id:stack) =
        modify (\s -> s { _stFunctions = M.add id functionBody }) >> byteExec instr stack
    byteExec (Sequence intructions : rest) stack = go instructions stack >>= byteExec rest

#endif

