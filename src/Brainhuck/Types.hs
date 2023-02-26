{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}

module Brainhuck.Types where

import Data.Kind (Type)
import Control.Exception
import Data.Foldable (foldlM)

data BrainhuckException
  = InexistentCellValueException
  | InexistentBenchmarkingInput
  deriving (Show)

instance Exception BrainhuckException

type Pointer = Int

data Instruction where
  IncPointer :: Instruction   --  >
  DecPointer :: Instruction   --  <
  IncCell    :: Instruction   --  +
  DecCell    :: Instruction   --  -
  GetChar    :: Instruction   --  ,
  PutChar    :: Instruction   --  .
  Loop       :: BFInstructionList t => t Instruction -> Instruction

newtype BFTest a = MkBFTest a
  deriving Functor

instance Applicative BFTest where
  pure = MkBFTest
  (MkBFTest f) <*> (MkBFTest a) = MkBFTest $ f a

instance Monad BFTest where
  (MkBFTest a) >>= f = f a

instance BFMonad BFTest
instance BFMonad IO


class Monad m => BFMonad m


pCharDebug            :: BFMonad m => m ()
pCharDebug = pure ()

class (Num cell, Eq cell) =>
      BFMemory (mem :: Type) (cell :: Type) | mem -> cell where

  gChar            :: mem -> Pointer -> IO mem
  pChar            :: mem -> Pointer -> IO ()

  gCharDebug       :: BFMonad m => Char -> mem -> Pointer -> m mem

  getCurrCellValue :: mem -> Pointer -> cell

  modifyCellValue  :: (cell -> cell -> cell)
                   -> mem -> Pointer -> mem

  currentCellIsZero :: mem -> Pointer -> Bool
  currentCellIsZero mem ptr = cellValue == 0
    where cellValue = getCurrCellValue mem ptr

  incCellValue :: mem -> Pointer -> mem
  incCellValue = modifyCellValue (+)

  decCellValue :: mem -> Pointer -> mem
  decCellValue = modifyCellValue (+)

class Foldable f => BFInstructionList f

class BFMonad m => BFState m state | m -> state, state -> m where
  incPointer :: state -> m state
  decPointer :: state -> m state
  incCell    :: state -> m state
  decCell    :: state -> m state
  getCharST  :: state -> m state
  putCharST  :: state -> m state
  currentCellIsZeroST :: state -> Bool
  initializeState' :: Int -> String {- The input if the state debugs -} -> state

executeInstruction' :: (BFState m state)
                    => state -> Instruction -> m state
executeInstruction' state instruction =
  case instruction of
     IncPointer -> incPointer state
     DecPointer -> decPointer state
     IncCell    -> incCell state
     DecCell    -> decCell state
     GetChar    -> getCharST state
     PutChar    -> putCharST state
     Loop prog  -> loopST prog state

loopST :: (BFInstructionList program, BFState m state)
       => program Instruction -> state -> m state
loopST  program pST =
  if currentCellIsZeroST pST
  then pure pST -- exits loop
  else interpret pST program >>= loopST program

interpret :: (BFInstructionList t, BFState m state)
          => state -> t Instruction -> m state
interpret = foldlM executeInstruction'
