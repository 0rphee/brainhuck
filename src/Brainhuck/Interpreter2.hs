{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Brainhuck.Interpreter2 where

import qualified Data.Vector.Unboxed as VU
import qualified Data.Sequence as S
import Control.Exception
import Data.Maybe (fromMaybe)
-- import Brainhuck.Parsing (parseProgram)
import Control.Monad (void)
import Data.Word (Word8)
import Brainhuck.Types

-- =====================================================================
-- Types
-- type AltMem = VU.MVector (PrimState IO) MemoryCell

init' :: (VU.Unbox a, Num a) => Int -> VU.Vector a
init' memSize = VU.replicate memSize 0


data ParsingError = BracketsNotClosed
  deriving Show

type MemoryCell = Word8


newtype MemoryVector a = MkMemoryVector (VU.Vector a)

type Memory = MemoryVector MemoryCell


newtype InstructionSeq a = MkInstructionSeq (S.Seq a) deriving Foldable

type Program = InstructionSeq Instruction


data ProgramState = MkState Memory Pointer

data ProgramStateDebug = MkStateDebug [Char] Memory Pointer

-- =====================================================================
-- Interpret 

instance BFInstructionList InstructionSeq

instance BFState BFTestMonad ProgramStateDebug where
  incPointer (MkStateDebug input mem ptr) =  pure $ MkStateDebug input mem (ptr+1)
  decPointer (MkStateDebug input mem ptr) =  pure $ MkStateDebug input mem (ptr-1)
  incCell    (MkStateDebug input mem ptr) = let modifiedMem = incCellValue mem ptr
                                             in pure $ MkStateDebug input modifiedMem ptr
  decCell    (MkStateDebug input mem ptr) = let modifiedMem = decCellValue mem ptr
                                             in pure $ MkStateDebug input modifiedMem ptr
  getCharST (MkStateDebug input mem ptr) = case input of
    [] -> throw InexistentBenchmarkingInput
    (x:xs) -> let modifiedMem = gCharDebug x mem ptr
              in pure $ MkStateDebug xs modifiedMem ptr
  putCharST = pure
  currentCellIsZeroST (MkStateDebug _ mem ptr) = currentCellIsZero mem ptr

initializeProgramStateDebug :: Int -> [Char] -> ProgramStateDebug
initializeProgramStateDebug memSize input = MkStateDebug input cells 0
  where cells = MkMemoryVector $ VU.replicate memSize 0


instance BFState IO ProgramState where
  incPointer (MkState mem ptr) = pure $ MkState mem (ptr+1)
  decPointer (MkState mem ptr) = pure $ MkState mem (ptr-1)
  incCell    (MkState mem ptr) = let modifiedMem = incCellValue mem ptr
                                  in pure $ MkState modifiedMem ptr
  decCell    (MkState mem ptr) = let modifiedMem = decCellValue mem ptr
                                  in pure $ MkState modifiedMem ptr
  getCharST (MkState mem ptr) = do
    modifiedMem <- gChar mem ptr
    pure $ MkState modifiedMem ptr

  putCharST st@(MkState mem ptr) = pChar mem ptr >> pure st
  currentCellIsZeroST (MkState mem ptr) = currentCellIsZero mem ptr

initializeProgramState :: Int -> ProgramState
initializeProgramState memSize = MkState cells 0
  where cells = MkMemoryVector $ VU.replicate memSize 0

tryToInterpret :: BFState m state => String -> state -> IO ()
tryToInterpret programString state = case parseProgram programString of
  Left err -> print err
  Right prog -> void $ exitToIO ( interpret state prog)

-- =====================================================================
-- Execution of Brainfuck operations 

instance BFMemory Memory MemoryCell where
  gChar (MkMemoryVector mem) ptr = do
    charVal <- fromIntegral . fromEnum <$> getChar
    putStrLn ""
    let modifiedMem = mem VU.// [(ptr, charVal)]
    pure (MkMemoryVector modifiedMem)

  pChar mem ptr = putChar $ (toEnum . fromIntegral) cellVal
    where cellVal = getCurrCellValue mem ptr

  gCharDebug char (MkMemoryVector mem) ptr = MkMemoryVector modifiedMem
    where charVal = (fromIntegral . fromEnum) char
          modifiedMem = mem VU.// [(ptr, charVal)]

  currentCellIsZero :: Memory -> Pointer -> Bool
  currentCellIsZero mem ptr = cellValue == 0
    where cellValue = getCurrCellValue mem ptr

  modifyCellValue :: (MemoryCell -> MemoryCell -> MemoryCell)
                  -> Memory -> Pointer -> Memory
  modifyCellValue operation (MkMemoryVector mem) ptr =
    MkMemoryVector $ VU.accum operation mem [(ptr, 1)]

  incCellValue :: Memory -> Pointer -> Memory
  incCellValue = modifyCellValue (+)

  decCellValue :: Memory -> Pointer -> Memory
  decCellValue = modifyCellValue (-)

  getCurrCellValue :: Memory -> Pointer -> MemoryCell
  getCurrCellValue (MkMemoryVector mem) ptr
    = fromMaybe (throw InexistentCellValueException) (mem VU.!? ptr)

-- =====================================================================
-- Parsing

parseProgram :: String -> Either ParsingError Program
parseProgram strProgram = snd <$> go False strProgram (MkInstructionSeq S.empty)
  where go :: Bool -> String -> Program -> Either ParsingError (String, Program)
        go loopOpen [] (MkInstructionSeq instructions) =  if loopOpen  -- if the loop is open, the only valid condition to exit it, is with ']'
                                        then Left BracketsNotClosed
                                        else Right ("", MkInstructionSeq instructions)
        go  loopOpen (x:xs) (MkInstructionSeq instructions)
          = let addCommonInstruction inst = go loopOpen xs $ MkInstructionSeq $ instructions S.|> inst
             in case x of
                 '>' -> addCommonInstruction IncPointer
                 '<' -> addCommonInstruction DecPointer
                 '+' -> addCommonInstruction IncCell
                 '-' -> addCommonInstruction DecCell
                 ',' -> addCommonInstruction GetChar
                 '.' -> addCommonInstruction PutChar
                 '[' -> case go True xs (MkInstructionSeq S.empty) of
                          Right (accum, instructs) ->
                            go  loopOpen accum $ MkInstructionSeq $ instructions S.|> Loop instructs
                          left -> left

                 ']' -> if loopOpen  -- if the loop is open, the only valid condition to exit it, is with ']'
                        then Right (xs, MkInstructionSeq instructions)
                        else Left BracketsNotClosed
                 _   -> go  loopOpen xs (MkInstructionSeq instructions)


