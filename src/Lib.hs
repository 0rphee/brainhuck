module Lib
  (
    helloWorld
  ) where

import qualified Data.Vector as V
import Data.Word ( Word8 )
import Data.Maybe ( fromMaybe )
import Control.Exception ( throw, Exception )
import Control.Monad ( void )
-- import Debug.Trace ( trace )

-- =====================================================================
-- TYPES 

-- | Exceptions used when the program is interpreted
data BrainhuckException
  = InexistentCellValueException
  | NoMatchingBracketException
  deriving (Show)

instance Exception BrainhuckException

type Pointer = Int
type MemoryCell = Word8

data ProgramState
  = MkState String        -- ^ The program to be executed
    (V.Vector MemoryCell) -- ^ The memory
    Pointer               -- ^ The pointer to the memory
  deriving Show

-- =====================================================================
-- Brainfuck operations 

-- | Primitive function to modify the `Pointer` in `ProgramState`
pointerOperation :: (Pointer -> Pointer) -> ProgramState -> ProgramState
pointerOperation op (MkState str mem ptr) = MkState str mem (op ptr)

incPointer :: ProgramState -> ProgramState
incPointer = pointerOperation (+1)

decPointer :: ProgramState -> ProgramState
decPointer = pointerOperation (\x -> x - 1)

-- Helper Function
getCurrCellValue :: V.Vector a -> Int -> a
getCurrCellValue mem ptr = fromMaybe (throw InexistentCellValueException) (mem V.!? ptr)

-- | Primitive function to modify the `MemoryCell` currently pointed at in `ProgramState`
cellOperation :: (MemoryCell -> MemoryCell) -> ProgramState -> ProgramState
cellOperation op (MkState str mem ptr) = MkState str (mem V.// [(ptr, op cellValue)]) ptr
  where cellValue = getCurrCellValue mem ptr


incCell :: ProgramState -> ProgramState
incCell = cellOperation (+1)

decCell :: ProgramState -> ProgramState
decCell = cellOperation (\x -> x - 1)

pChar :: ProgramState -> IO ProgramState
pChar (MkState str mem ptr) = do
  let cellValue = getCurrCellValue mem ptr
  putChar $ (toEnum . fromIntegral) cellValue -- converts to char the cellValue (int)
  pure $ MkState str mem ptr

gChar :: ProgramState -> IO ProgramState
gChar (MkState str mem ptr) = do
  readCharValue <- fromIntegral . fromEnum <$> getChar
  putChar '\n'
  let modifiedMem = mem V.// [(ptr, readCharValue)]
  pure $ MkState str modifiedMem ptr

currentCellIsZero :: ProgramState -> Bool
currentCellIsZero (MkState _ mem ptr) = cellValue == 0
  where cellValue = getCurrCellValue mem ptr

-- myTrace :: Show a => String -> a -> a
-- myTrace string other = trace (string <> show other) other 

loop :: String -> ProgramState -> IO ProgramState
loop carry pState =
  case pState of                                                    -- myTrace "pState in loop: "
    MkState [] _ _           -> throw NoMatchingBracketException
    cst@(MkState (']':xs) mem ptr) ->                               -- trace "\tinside (]:xs)" $ 
      if currentCellIsZero cst                                      -- myTrace "\tTerminaLoop: " $
      then execute $ MkState xs mem ptr
      else loop "" $ MkState (reverse carry <> "]" <> xs) mem ptr
    MkState (x:xs) mem ptr   ->                                     -- trace "\tinside (x:xs)"
      getProgramOperation x (MkState xs mem ptr) >>= loop (x:carry)

-- | The ']' command is "embeded" in the loop function
getProgramOperation :: Char -> (ProgramState -> IO ProgramState)
getProgramOperation char = case char of
  '>' -> pure . incPointer
  '<' -> pure . decPointer
  '+' -> pure . incCell
  '-' -> pure . decCell
  ',' -> gChar
  '.' -> pChar
  '[' -> loop ""
  _   -> pure

execute :: ProgramState -> IO ProgramState
execute (MkState [] mem ptr) = pure $ MkState [] mem ptr
execute (MkState (x:xs) mem ptr) = nextProgState >>= execute
  where nextProgState = getProgramOperation x $ MkState xs mem ptr

-- | Returns an initial `ProgramState` with the provided memory
--   size of 1 byte, initialized to 0 (the standard is of 30,000 
--   memory blocks).
initialState :: Int -> String -> ProgramState
initialState memSize program = MkState program cells 0
  where cells = V.replicate memSize 0


helloWorldProgram :: String
helloWorldProgram = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-] >++++++++[<++++>-] <.>+++++++++++[<++++++++>-]<-.--------.+++ .------.--------.[-]>++++++++[<++++>- ]<+.[-]++++++++++."

testState :: Bool -> ProgramState -> IO ()
testState printAll testSt = if printAll
                            then execute testSt >>= print
                            else void (execute testSt)

helloWorld :: IO ()
helloWorld = testState False $ initialState 10 helloWorldProgram


