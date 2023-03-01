{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}

module Brainhuck.Types 
  ( BFMemory(..)
  , Instruction(..)
  , Pointer
  , interpret
  , BFState(..)
  , BFInstructionList
  , BFTestMonad(..)
  , BFMonad(..)
  , BrainhuckError(..)
  , BFParsingError(..)
  , BFRuntimeError(..)
  ) where

import Data.Kind (Type)
import Data.Foldable (foldlM)
import Control.Monad.Trans.Except




data BFParsingError = BracketsNotClosed
  deriving Show

data BFRuntimeError 
  = InexistentCellValue
  | InexistentBenchmarkingInput
  deriving Show

data BrainhuckError
  = BrainHuckRuntimeError BFRuntimeError 
  | BrainHuckParsingError BFParsingError
  deriving Show


type Pointer = Int

data Instruction where
  IncPointer :: Instruction   --  >
  DecPointer :: Instruction   --  <
  IncCell    :: Instruction   --  +
  DecCell    :: Instruction   --  -
  GetChar    :: Instruction   --  ,
  PutChar    :: Instruction   --  .
  Loop       :: BFInstructionList t => t Instruction -> Instruction

data BFTestMonad a = MkBFTestMonad !a
  deriving Functor

instance Applicative BFTestMonad where
  pure = MkBFTestMonad
  (MkBFTestMonad f) <*> (MkBFTestMonad a) = MkBFTestMonad $ f a

instance Monad BFTestMonad where
  (MkBFTestMonad a) >>= f = f a

instance BFMonad BFTestMonad where
  exitToIO (MkBFTestMonad a) = return a
instance BFMonad IO where
  exitToIO = id


class Monad m => BFMonad m where
  exitToIO :: m a -> IO a


class (Num cell, Eq cell) =>
      BFMemory (mem :: Type) (cell :: Type) | mem -> cell where

  gChar            :: mem -> Pointer -> IO mem
  pChar            :: mem -> Pointer -> ExceptT BFRuntimeError IO ()

  gCharDebug       :: Char -> mem -> Pointer -> mem

  getCurrCellValue :: mem -> Pointer -> Either BFRuntimeError cell

  modifyCellValue  :: (cell -> cell -> cell)
                   -> mem -> Pointer -> mem

  currentCellIsZero :: mem -> Pointer -> Either BFRuntimeError Bool
  currentCellIsZero mem ptr = do
    cellValue <- getCurrCellValue mem ptr
    pure $ cellValue == 0

  incCellValue :: mem -> Pointer -> mem
  incCellValue = modifyCellValue (+)

  decCellValue :: mem -> Pointer -> mem
  decCellValue = modifyCellValue (+)

class Foldable f => BFInstructionList f

class BFMonad m => BFState m state | state -> m where
  incPointer :: state -> m state
  decPointer :: state -> m state
  incCell    :: state -> m state
  decCell    :: state -> m state
  getCharST  :: state -> ExceptT BFRuntimeError m state
  putCharST  :: state -> ExceptT BFRuntimeError m state
  currentCellIsZeroST :: state -> Either BFRuntimeError Bool
  -- initializeState' :: Int -> String {- The input if the state debugs -} -> state

executeInstruction :: (BFState m state)
                    => state -> Instruction -> ExceptT BFRuntimeError m state
executeInstruction state instruction =
  case instruction of
     IncPointer -> ExceptT $ Right <$> incPointer state
     DecPointer -> ExceptT $ Right <$> decPointer state
     IncCell    -> ExceptT $ Right <$> incCell state
     DecCell    -> ExceptT $ Right <$> decCell state
     GetChar    -> getCharST state
     PutChar    -> putCharST state
     Loop prog  -> loopST prog state

loopST :: (BFInstructionList program, BFState m state)
       => program Instruction -> state -> ExceptT BFRuntimeError m state
loopST  program pST =
  case currentCellIsZeroST pST of
  Left err -> except $ Left err
  Right cellIsZero -> if cellIsZero
                      then pure pST -- exits loop
                      else interpret pST program >>= loopST program

interpret :: (BFInstructionList t, BFState m state)
          => state -> t Instruction -> ExceptT BFRuntimeError m state
interpret = foldlM executeInstruction
