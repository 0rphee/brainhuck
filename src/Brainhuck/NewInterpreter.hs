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




-- =====================================================================
-- Execution of Brainfuck operations 

-- | Primitive function to modify the `Pointer` in `ProgramState`
pointerOperation :: (Pointer -> Pointer) -> ProgramState -> ProgramState
pointerOperation operation (MkState prog mem ptr)
  = MkState prog mem (operation ptr)

incPointer :: ProgramState -> ProgramState
incPointer = pointerOperation (+1)

decPointer :: ProgramState -> ProgramState
decPointer = pointerOperation (\x -> x - 1)

-- | Helper Function
getCurrCellValue :: Memory -- ^ the Memory Vector
  -> Pointer -- ^ index currently pointed at 
  -> MemoryCell
getCurrCellValue mem ptr
  = fromMaybe (throw InexistentCellValueException) (mem V.!? ptr)

-- | Primitive function to modify the `MemoryCell` currently pointed at in `ProgramState`
cellOperation :: (MemoryCell -> MemoryCell) -> ProgramState -> ProgramState
cellOperation op (MkState str mem ptr) = MkState str (mem V.// [(ptr, op cellValue)]) ptr
  where cellValue = getCurrCellValue mem ptr

-- | Increment Cell value in current pointer value
incCell :: ProgramState -> ProgramState
incCell = cellOperation (+1)

-- | Decrement Cell value in current pointer value
decCell :: ProgramState -> ProgramState
decCell = cellOperation (\x -> x - 1)

-- | Value in current pointer location is printed
pChar :: ProgramState -> IO ProgramState
pChar (MkState str mem ptr) = do
  let cellValue = getCurrCellValue mem ptr
  putChar $ (toEnum . fromIntegral) cellValue -- converts to char the cellValue (int)
  pure $ MkState str mem ptr

-- | Input from the user is saved in pointer location
gChar :: ProgramState -> IO ProgramState
gChar (MkState str mem ptr) = do
  readCharValue <- fromIntegral . fromEnum <$> getChar
  putChar '\n'
  let modifiedMem = mem V.// [(ptr, readCharValue)]
  pure $ MkState str modifiedMem ptr

-- | Validates if currently pointed at location is 0
currentCellIsZero :: ProgramState -> Bool
currentCellIsZero (MkState _ mem ptr) = cellValue == 0
  where cellValue = getCurrCellValue mem ptr


