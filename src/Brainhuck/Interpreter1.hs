{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Brainhuck.Interpreter1
  (
    runBF
  , initializeProgramState
  , initializeProgramStateDebug
  ) where

import qualified Data.Vector as V
import qualified Data.Sequence as S
import Data.Word (Word8)
import Brainhuck.Types
import qualified Data.Text as T
import Control.Monad.Trans.Except
import Control.DeepSeq

-- =====================================================================
-- Types
-- type AltMem = VU.MVector (PrimState IO) MemoryCell

-- init' :: (VU.Unbox a, Num a) => Int -> VU.Vector a
-- init' memSize = VU.replicate memSize 0




type MemoryCell = Word8


newtype MemoryVector a = MkMemoryVector (V.Vector a)

type Memory = MemoryVector MemoryCell


newtype InstructionSeq a = MkInstructionSeq (S.Seq a) deriving (Foldable, Show)

type Program = InstructionSeq Instruction


data ProgramState = MkState Memory Pointer

data ProgramStateDebug = MkStateDebug [Char] Memory Pointer

instance NFData ProgramStateDebug where
  rnf a = seq a ()
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
    [] -> except $ Left InexistentBenchmarkingInput
    (x:xs) -> let modifiedMem = gCharDebug x mem ptr
              in pure $ MkStateDebug xs modifiedMem ptr
  -- getCharST (MkStateDebug input mem ptr) 
  --   = case input of
  --     [] -> except $ Left InexistentBenchmarking   
  --     (x:xs) -> 
  
  putCharST = pure
  currentCellIsZeroST (MkStateDebug _ mem ptr) = currentCellIsZero mem ptr

initializeProgramStateDebug :: Int -> [Char] -> ProgramStateDebug
initializeProgramStateDebug memSize input = MkStateDebug input cells 0
  where cells = MkMemoryVector $ V.replicate memSize 0


instance BFState IO ProgramState where
  incPointer (MkState mem ptr) = pure $ MkState mem (ptr+1)
  decPointer (MkState mem ptr) = pure $ MkState mem (ptr-1)
  incCell    (MkState mem ptr) = let modifiedMem = incCellValue mem ptr
                                  in pure $ MkState modifiedMem ptr
  decCell    (MkState mem ptr) = let modifiedMem = decCellValue mem ptr
                                  in pure $ MkState modifiedMem ptr
  getCharST (MkState mem ptr) = do
    modifiedMem <- except $ Right $ gChar mem ptr
    ExceptT $ Right . (`MkState` ptr) <$> modifiedMem

  putCharST st@(MkState mem ptr) = do
    pChar mem ptr
    return st

  currentCellIsZeroST (MkState mem ptr) = currentCellIsZero mem ptr

initializeProgramState :: Int -> ProgramState
initializeProgramState memSize = MkState cells 0
  where cells = MkMemoryVector $ V.replicate memSize 0


runBF :: BFState m state => T.Text -> state -> IO ()
runBF programString state = do
  result <- exitToIO . runExceptT $ tryToInterpret programString state
  case result of
    Left err -> print err
    Right _  -> pure ()

tryToInterpret :: BFState m state => T.Text -> state -> ExceptT BrainhuckError m state
tryToInterpret programString state = case parseProgram programString of
  Left err -> except $ Left $ BrainHuckParsingError err
  Right prog -> withExceptT BrainHuckRuntimeError (interpret state prog)
-- =====================================================================
-- Execution of Brainfuck operations 

instance BFMemory Memory MemoryCell where
  gChar (MkMemoryVector mem) ptr = do
    charVal <- fromIntegral . fromEnum <$> getChar
    putStrLn ""
    let modifiedMem = mem V.// [(ptr, charVal)]
    pure (MkMemoryVector modifiedMem)

  pChar mem ptr = do
   cellVal <- except $ getCurrCellValue mem ptr
   ExceptT $ Right <$> putChar ((toEnum . fromIntegral) cellVal)

  -- pChar mem ptr = do
  --   cellVal <- except $ getCurrCellValue mem ptr
  --   putChar $ (toEnum . fromIntegral) cellVal
  --   pure ()

  gCharDebug char (MkMemoryVector mem) ptr = MkMemoryVector modifiedMem
    where charVal = (fromIntegral . fromEnum) char
          modifiedMem = mem V.// [(ptr, charVal)]

  modifyCellValue :: (MemoryCell -> MemoryCell -> MemoryCell)
                  -> Memory -> Pointer -> Memory
  modifyCellValue operation (MkMemoryVector mem) ptr =
    MkMemoryVector $ V.accum operation mem [(ptr, 1)]

  incCellValue :: Memory -> Pointer -> Memory
  incCellValue = modifyCellValue (+)

  decCellValue :: Memory -> Pointer -> Memory
  decCellValue = modifyCellValue (-)

  getCurrCellValue (MkMemoryVector mem) ptr
    = case mem V.!? ptr of
      Nothing -> Left InexistentCellValue
      Just cellVal -> Right cellVal

-- =====================================================================
-- Parsing

parseProgram :: T.Text -> Either BFParsingError Program
parseProgram strProgram = snd <$> go False strProgram (MkInstructionSeq S.empty)
  where go :: Bool -> T.Text -> Program -> Either BFParsingError (T.Text, Program)
        go loopOpen text (MkInstructionSeq instructions)
          = case T.uncons text of
            Nothing -> if loopOpen
                       then Left BracketsNotClosed
                       else Right ("", MkInstructionSeq instructions)
            Just (x, xs) ->
              let addCommonInstruction inst = go loopOpen xs $ MkInstructionSeq $ instructions S.|> inst
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

