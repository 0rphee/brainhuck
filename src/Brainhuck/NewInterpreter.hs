module Brainhuck.NewInterpreter () where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Word (Word8)
import Control.Exception
import Data.Maybe (fromMaybe)

-- =====================================================================
-- Types

data BrainhuckException
  = InexistentCellValueException
  | NoMatchingBracketException
  deriving (Show)

instance Exception BrainhuckException

type Pointer = Int
type MemoryCell = Word8

type Memory = V.Vector MemoryCell

newtype Program = Program (S.Seq Instruction)
  deriving Show

data Instruction
  = IncPointer    --  >
  | DecPointer    --  <
  | IncCell       --  +
  | DecCell       --  -
  | GetChar       --  ,
  | PutChar       --  .
  | Loop Program  --  [
instance Show Instruction where
  show IncPointer  = '_' : (">"    <>" ")
  show DecPointer  = '_' : ("<"    <>" ")
  show IncCell     = '_' : ("+"    <>" ")
  show DecCell     = '_' : ("-"    <>" ")
  show GetChar     = '_' : (","    <>" ")
  show PutChar     = '_' : ("."    <>" ")
  show (Loop prog) = '_' : ("LOOP" <>" " <> show prog)

data ProgramState
  = MkState
      { getProgram :: Program
      , getMemory  :: Memory
      , getPointer :: Pointer
      }

data Error = BracketsNotClosed
  deriving Show

-- =====================================================================
-- Parsing

parseProgram :: String -> Either Error Program
parseProgram strProgram = Program . snd <$> go False strProgram S.empty
  where go :: Bool -> String -> S.Seq Instruction -> Either Error (String, S.Seq Instruction)
        go isLoopOpen [] instructions = if isLoopOpen
                                        then Right ("", instructions)
                                        else Left BracketsNotClosed
        go isLoopOpen (x:xs) instructions 
          = let addCommonInstruction inst = go isLoopOpen xs $ instructions S.|> inst
             in case x of
                 '>' -> addCommonInstruction IncPointer
                 '<' -> addCommonInstruction DecPointer
                 '+' -> addCommonInstruction IncCell
                 '-' -> addCommonInstruction DecCell
                 ',' -> addCommonInstruction GetChar
                 '.' -> addCommonInstruction PutChar
                 '[' -> case go True xs S.empty of
                          Left e -> Left e
                          Right (accum, instructs) -> 
                            go isLoopOpen accum $ instructions S.|> Loop (Program instructs)
         
                 ']' -> if isLoopOpen
                        then Right (xs, instructions)
                        else Left BracketsNotClosed
                 _   -> go isLoopOpen xs instructions
