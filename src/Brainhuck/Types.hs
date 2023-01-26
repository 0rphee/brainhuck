module Brainhuck.Types
  (
    BrainhuckException(..)
  , ParsingError(..)
  , Pointer
  , MemoryCell
  , Memory
  , Instruction(..)
  , Program(..)
  , ProgramState(..)
  , BFState (..)
  , BenchState(..)
  )
  where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Word (Word8)
import Data.Foldable (toList)
import Control.Exception

-- =====================================================================
-- Types

data BrainhuckException
  = InexistentCellValueException
  | InexistentBenchmarkingInput
  deriving (Show)

instance Exception BrainhuckException

data ParsingError = BracketsNotClosed
  deriving Show

type Pointer = Int
type MemoryCell = Word8

type Memory = V.Vector MemoryCell

data Instruction
  = IncPointer    --  >
  | DecPointer    --  <
  | IncCell       --  +
  | DecCell       --  -
  | GetChar       --  ,
  | PutChar       --  .
  | Loop Program  --  [

newtype Program = Program (S.Seq Instruction)

instance Show Program where
  show (Program seqq) = concatMap show $ toList seqq

instance Show Instruction where
  show IncPointer  = ">"
  show DecPointer  = "<"
  show IncCell     = "+"
  show DecCell     = "-"
  show GetChar     = ","
  show PutChar     = "."
  show (Loop prog) = " LOOP["  <> show prog <> "]"

class BFState state where
  executeInstruction :: state -> Instruction -> IO state
  initializeState :: Int -> String -> state

data BenchState = BenchState String Memory Pointer

data ProgramState
  = MkState Memory Pointer

