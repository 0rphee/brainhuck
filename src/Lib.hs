{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Lib where

import Prelude hiding ( replicate )

import qualified Data.Vector as V
import Data.Word (Word8, Word16)
import Data.Maybe (fromMaybe)
import Control.Monad.Trans.State.Lazy ( StateT(StateT) )
import Control.Exception ( throw, Exception )
import Data.List (replicate)

data BrainhuckException
  = InexistentCellValueException
  | NoMatchingBracketException
  deriving (Show)

instance Exception BrainhuckException

type Pointer = Word16
type MemoryCell = Word8

data ProgramState
  = MkState String (V.Vector MemoryCell) Pointer
  deriving Show

pointerOperation :: (Pointer -> Pointer) -> ProgramState -> ProgramState
pointerOperation op (MkState str mem ptr) = MkState str mem (op ptr)

incPointer :: ProgramState -> ProgramState
incPointer = pointerOperation (+1)

decPointer :: ProgramState -> ProgramState
decPointer = pointerOperation (\x -> x - 1)

getCurrCellValue :: V.Vector a -> Int -> a
getCurrCellValue mem intPtr = fromMaybe (throw InexistentCellValueException) (mem V.!? intPtr)

cellOperation :: (MemoryCell -> MemoryCell) -> ProgramState -> ProgramState
cellOperation op (MkState str mem ptr) = MkState str (mem V.// [(intPtr, op cellValue)]) ptr
  where intPtr = fromIntegral ptr
        cellValue = getCurrCellValue mem intPtr


incCell :: ProgramState -> ProgramState
incCell = cellOperation (+1)

decCell :: ProgramState -> ProgramState
decCell = cellOperation (\x -> x - 1)

pChar :: ProgramState -> IO ProgramState
pChar (MkState str mem ptr) = do
  let intPtr = fromIntegral ptr
  let cellValue = fromIntegral $ fromMaybe (throw InexistentCellValueException) (mem V.!? intPtr)
  putChar $ toEnum cellValue -- converts to char the cellValue (int)
  putStrLn ""
  pure $ MkState str mem ptr

gChar :: ProgramState -> IO ProgramState
gChar (MkState str mem ptr) = do
  readCharValue <- fromIntegral . fromEnum <$> getChar
  putChar '\n'
  let intPtr = fromIntegral ptr
  let modifiedMem = mem V.// [(intPtr, readCharValue)]
  pure $ MkState str modifiedMem ptr

currentCellIsZero :: ProgramState -> Bool
currentCellIsZero (MkState _ mem ptr) = cellValue == 0
  where intPtr = fromIntegral ptr
        cellValue = fromMaybe (throw InexistentCellValueException) (mem V.!? intPtr)

loop :: String -> ProgramState -> IO ProgramState
loop _     (MkState [] _ _) = throw NoMatchingBracketException
loop carry (MkState (']':xs) mem ptr)
-- modify all of the function
  | endLoop = pure midProgState
  | otherwise = finalProgState >>= loop (carry++[x]) -- TODO
  where midProgState = MkState xs mem ptr
        finalProgState = getProgramOperation x midProgState
        endLoop = x == ']' && currentCellIsZero midProgState

getProgramOperation :: Char -> (ProgramState -> IO ProgramState)
getProgramOperation char = case char of
  '>' -> pure . incPointer
  '<' -> pure . decPointer
  '+' -> pure . incCell
  '-' -> pure . decCell
  ',' -> gChar
  '.' -> pChar
  '[' -> undefined -- TODO: implement loop
  _   -> pure
-- execute :: ProgramState -> IO ( (), ProgramState )
-- execute :: StateT ProgramState IO ()
-- execute = StateT $  \ case
--     MkState [] mem ptr     -> pure ( (), MkState [] mem ptr )
--     MkState (x:xs) mem ptr -> let nextProgState = getProgramOperation x $ MkState xs mem ptr
--                               in  ( (), ) <$> nextProgState

execute :: ProgramState -> IO ProgramState
execute (MkState [] mem ptr) = pure $ MkState [] mem ptr
execute (MkState (x:xs) mem ptr) = let nextProgState = getProgramOperation x $ MkState xs mem ptr
                                    in nextProgState >>= execute

initialState :: String -> ProgramState
initialState str = MkState str cells 0
  where cells = V.replicate 30 0 -- array size of 30,0000 memory blocks of 1 byte, initialized to 0

testingState :: ProgramState
testingState = initialState $ replicate 65 '+' <> "."



test :: Bool -> IO ()
test printAll = if printAll
                then execute testingState >>= print
                else execute testingState >> putStrLn ""

someFunc :: IO ()
someFunc = putStrLn "someFunc"
