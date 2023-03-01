{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

module Brainhuck.Interpreter1
  (
    runBF
  , runBF'
  , initializeProgramState
  , initializeProgramStateDebug
  , ProgramStateDebug(..)
  -- , optimize
  -- , parseProgram'
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
-- type AltMem = V.MVector (PrimState IO) MemoryCell

-- init' :: (V.Unbox a, Num a) => Int -> V.Vector a
-- init' memSize = V.replicate memSize 0




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
instance BFInstructionList InstructionL

instance BFState BFTestMonad ProgramStateDebug where
  modifyPointerST val (MkStateDebug input mem ptr) =  pure $ MkStateDebug input mem (ptr+val)
  modifyCellST    val (MkStateDebug input mem ptr)
    = let modifiedMem = modifyCellValue val mem ptr
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
  modifyPointerST val (MkState mem ptr) =  pure $ MkState mem (ptr+val)
  modifyCellST    val (MkState mem ptr)
    = let modifiedMem = modifyCellValue val mem ptr
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

  modifyCellValue :: Int
                  -> Memory -> Pointer -> Memory
  modifyCellValue val (MkMemoryVector mem) ptr 
    | 0 < val = MkMemoryVector $ V.accum (+) mem [(ptr, fromIntegral val)]
    | otherwise = MkMemoryVector $ V.accum (-) mem [(ptr, fromIntegral (negate val))]


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
                 '>' -> addCommonInstruction $ ModifyPointer 1
                 '<' -> addCommonInstruction $ ModifyPointer (negate 1)
                 '+' -> addCommonInstruction $ ModifyCell 1
                 '-' -> addCommonInstruction $ ModifyCell (negate 1)
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

newtype InstructionL a = MkInstructionL [a] deriving (Foldable, Show)
type ProgramL = InstructionL Instruction

parseProgram' :: T.Text -> Either BFParsingError ProgramL
parseProgram' strProgram =reverse'. snd <$> go False strProgram (MkInstructionL [])
  where go :: Bool -> T.Text -> ProgramL -> Either BFParsingError (T.Text, ProgramL)
        go loopOpen text (MkInstructionL instructions)
          = case T.uncons text of
            Nothing -> if loopOpen
                       then Left BracketsNotClosed
                       else Right ("", MkInstructionL instructions)
            Just (x, xs) ->
              let addCommonInstruction inst = go loopOpen xs $ MkInstructionL $ inst : instructions
              in case x of
                 '>' -> addCommonInstruction $ ModifyPointer 1
                 '<' -> addCommonInstruction $ ModifyPointer (negate 1)
                 '+' -> addCommonInstruction $ ModifyCell 1
                 '-' -> addCommonInstruction $ ModifyCell (negate 1)
                 ',' -> addCommonInstruction GetChar
                 '.' -> addCommonInstruction PutChar
                 '[' -> case go True xs (MkInstructionL []) of
                          Right (accum, instructs) ->
                            go  loopOpen accum $ MkInstructionL $ Loop instructs : instructions
                          left -> left

                 ']' -> if loopOpen  -- if the loop is open, the only valid condition to exit it, is with ']'
                        then Right (xs, MkInstructionL instructions)
                        else Left BracketsNotClosed
                 _   -> go  loopOpen xs (MkInstructionL instructions)
        reverse' (MkInstructionL l) = MkInstructionL $ reverse l

optimize :: ProgramL -> ProgramL
optimize (MkInstructionL l) = MkInstructionL $ go Nothing l
  where go :: Maybe Instruction -> [Instruction] -> [Instruction]
        go (Just x) [] = case x of
          ModifyCell    0 -> []
          ModifyPointer 0 -> []
          _               -> [x]
        go Nothing [] = []
        go Nothing (x:xs) = go (Just x) xs
        go (Just x) (y:ys)
          = case (x, y) of
            (ModifyPointer 0, _) ->                   go                      Nothing ys
            (ModifyCell 0   , _) ->                   go                      Nothing ys
            (ModifyPointer a, ModifyPointer b) ->     go (Just $ ModifyPointer (a+b)) ys
            (ModifyCell a   , ModifyCell    b) ->     go (Just $ ModifyCell    (a+b)) ys
            (_              ,               _) -> x : go (Just                    y) ys


runBF' :: BFState m state => T.Text -> state -> IO ()
runBF' programString state = do
  result <- exitToIO . runExceptT $ tryToInterpret programString state
  case result of
    Left err -> print err
    Right _  -> pure ()

tryToInterpret' :: BFState m state => T.Text -> state -> ExceptT BrainhuckError m state
tryToInterpret' programString state = case parseProgram' programString of
  Left err -> except $ Left $ BrainHuckParsingError err
  Right prog -> withExceptT BrainHuckRuntimeError (interpret state (optimize prog))
