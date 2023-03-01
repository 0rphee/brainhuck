{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE StandaloneDeriving #-}

module Brainhuck.Types 
  ( BFMemory(..)
  , Instruction(..)
  , Pointer
  , BFState(..)
  , BFInstructionList
  , BFTestMonad(..)
  , BFMonad(..)
  , BrainhuckError(..)
  , BFParsingError(..)
  , BFRuntimeError(..)
  , interpret
  , executeInstruction
  ) where

import Data.Kind (Type)
import Data.Foldable (foldlM)
import Control.Monad.Trans.Except
import Control.DeepSeq

data BFParsingError = BracketsNotClosed
  deriving Show

data BFRuntimeError 
  = InexistentCellValue
  | InexistentBenchmarkingInput
  deriving Show

instance NFData BFRuntimeError where
  rnf a = seq a ()

data BrainhuckError
  = BrainHuckRuntimeError BFRuntimeError 
  | BrainHuckParsingError BFParsingError
  deriving Show


type Pointer = Int

data Instruction where
  IncPointer  :: Instruction   --  >
  DecPointer  :: Instruction   --  <
  IncCell     :: Instruction   --  +
  DecCell     :: Instruction   --  -

  IncPointer' :: Int -> Instruction   --  >
  DecPointer' :: Int -> Instruction   --  <
  IncCell'    :: (Show cell, Num cell) => cell -> Instruction   --  +
  DecCell'    :: (Show cell, Num cell) => cell -> Instruction   --  -

  GetChar     :: Instruction   --  ,
  PutChar     :: Instruction   --  .
  Loop        :: (BFInstructionList t, Show (t Instruction)) => t Instruction -> Instruction



instance Show Instruction where
  show IncPointer      = ">"
  show DecPointer      = "<"
  show IncCell         = "+"
  show DecCell         = "-"
  show (IncPointer' x) = ">" ++ show x
  show (DecPointer' x) = "<" ++ show x
  show (IncCell'    x) = "+" ++ show x
  show (DecCell'    x) = "-" ++ show x
  show GetChar         = ","
  show PutChar         = "."
  show (Loop prog)     = " LOOP["  <> show prog <> "]"


data BFTestMonad a = MkBFTestMonad !a
  deriving Functor

instance Applicative BFTestMonad where
  pure = MkBFTestMonad
  (MkBFTestMonad f) <*> (MkBFTestMonad a) = MkBFTestMonad $ f a

instance Monad BFTestMonad where
  (MkBFTestMonad a) >>= f = f a

instance BFMonad BFTestMonad where
  exitToIO (MkBFTestMonad a) = pure a
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
