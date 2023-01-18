module Brainhuck.NewInterpreter where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Word (Word8)

type Pointer = Int
type MemoryCell = Word8

type Memory = V.Vector MemoryCell

newtype Program = Program (S.Seq Instruction)

data Instruction 
  = IncPointer    --  >
  | DecPointer    --  <
  | IncCell       --  +
  | DecCell       --  -
  | GetChar       --  ,
  | PutChar       --  .
  | Loop Program  -- [

data ProgramState 
  = PGST 
      { getProgram :: Program
      , getMemory  :: Memory
      , getPointer :: Pointer  
      } 


pointerOperation :: (Pointer -> Pointer) -> ProgramState -> ProgramState
pointerOperation operation (PGST prog mem ptr) = PGST prog mem (operation ptr)





-- data ProgramState = MkState { program :: []}

