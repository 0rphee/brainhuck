module Brainhuck.NewInterpreter
  (
    interpret
  , Error(..)
  , Program(..)
  , ProgramState
  , Instruction(..)
  , BrainhuckException(..)
  , initState
  ,
  )
  where

import qualified Data.Sequence as S
import qualified Data.Vector as V
import Data.Word (Word8)
import Control.Exception
import Data.Maybe (fromMaybe)
import Data.Foldable (foldlM, Foldable (toList))

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

data ProgramState
  = MkState Memory Pointer
      -- { getMemory  :: Memory
      -- , getPointer :: Pointer
      -- }

data Error = BracketsNotClosed
  deriving Show


-- =====================================================================
-- Interpret 

initState :: Int -- ^ memory size
          -> ProgramState
initState memSize = MkState cells 0
 where cells = V.replicate memSize 0


myFoldM :: Monad m
        => (ProgramState -> Instruction -> m ProgramState)
        -> ProgramState
        -> Program
        -> m ProgramState
myFoldM f initialState (Program instructions) =
-- foldlM :: (b -> a -> m b) -> b -> t a -> m b 
  foldlM f initialState instructions

exeInstruction :: ProgramState -> Instruction -> IO ProgramState
exeInstruction (MkState mem ptr) instruction =
  case instruction of
     IncPointer -> pure $ MkState mem (ptr+1)
     DecPointer -> pure $ MkState mem (ptr-1)
     IncCell    -> let modifiedMem = incCellValue mem ptr
                    in pure $ MkState modifiedMem ptr

     DecCell    -> let modifiedMem = decCellValue mem ptr
                    in pure $ MkState modifiedMem ptr

     GetChar    -> let modifiedMem = gChar mem ptr
                    in (`MkState` ptr) <$> modifiedMem

     PutChar    -> pChar mem ptr
                >> pure (MkState mem ptr )

     Loop prog  ->  loop prog (MkState mem ptr)

  where  loop :: Program -> ProgramState -> IO ProgramState
         loop loopProg pST@(MkState loopMem loopPtr') =
          if currentCellIsZero loopMem loopPtr' 
          then pure pST -- exits loop
          else interpret loopProg pST >>=  loop loopProg



interpret :: Program -> ProgramState -> IO ProgramState
interpret initialProgram initialState =
  myFoldM exeInstruction initialState initialProgram


-- =====================================================================
-- Execution of Brainfuck operations 

currentCellIsZero :: Memory -> Pointer -> Bool
currentCellIsZero mem ptr = cellValue == 0
  where cellValue = getCurrCellValue mem ptr

modifyCellValue :: (MemoryCell -> MemoryCell -> MemoryCell)
                -> Memory -> Pointer -> Memory
modifyCellValue operation mem ptr = V.accum operation mem [(ptr, 1)]

incCellValue :: Memory -> Pointer -> Memory
incCellValue = modifyCellValue (+)

decCellValue :: Memory -> Pointer -> Memory
decCellValue = modifyCellValue (-)

gChar :: Memory -> Pointer -> IO Memory
gChar mem ptr = do
  charVal <- fromIntegral . fromEnum <$> getChar
  putStrLn ""
  let modifiedMem = mem V.// [(ptr, charVal)]
  pure modifiedMem

pChar :: Memory -> Pointer -> IO ()
pChar mem ptr = do
  let cellVal = getCurrCellValue mem ptr
  putChar $ (toEnum . fromIntegral) cellVal -- converts to char the cellValue (int)

-- | Helper Function
getCurrCellValue :: Memory -- ^ the Memory Vector
  -> Pointer -- ^ index currently pointed at 
  -> MemoryCell
getCurrCellValue mem ptr
  = fromMaybe (throw InexistentCellValueException) (mem V.!? ptr)

