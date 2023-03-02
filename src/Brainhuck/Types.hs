{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DeriveFunctor #-}

module Brainhuck.Types
  ( BFMemory(..)
  , Instruction(..)
  , Pointer
  , BFState(..)
  , BFInstructionList(..)
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
import Data.Functor.Classes (Show1 (liftShowsPrec))

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

data Instruction t where
  ModifyPointer :: Int -> Instruction t   --  >
  ModifyCell    :: Int -> Instruction t  --  -
  GetChar     :: Instruction t   --  ,
  PutChar     :: Instruction t   --  .
  Loop        :: (BFInstructionList t, Show (t (Instruction t))) => t (Instruction t) -> (Instruction t)

instance Show (Instruction f) where
  show (ModifyPointer x) = '<' : show x ++ ">"
  show (ModifyCell    x) = '+' : show x ++ "-"
  show GetChar         = ","
  show PutChar         = "."
  show (Loop prog)     = " LOOP["  <> show prog <> "]"


data BFTestMonad a = MkBFTestMonad !a
  deriving (Functor)

instance Show1 BFTestMonad where
 liftShowsPrec shwsPre _ fixity (MkBFTestMonad a) = shwsPre fixity a 

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

  modifyCellValue  :: Int -> mem -> Pointer -> mem

  currentCellIsZero :: mem -> Pointer -> Either BFRuntimeError Bool
  currentCellIsZero mem ptr = do
    cellValue <- getCurrCellValue mem ptr
    pure $ cellValue == 0

class Foldable program => BFInstructionList program where
  optimize :: program (Instruction program) -> program (Instruction program)

class BFMonad m => BFState m state | state -> m where
  modifyPointerST :: Pointer -> state -> m state
  modifyCellST    :: Int -> state -> m state
  getCharST     :: state -> ExceptT BFRuntimeError m state
  putCharST     :: state -> ExceptT BFRuntimeError m state
  currentCellIsZeroST :: state -> Either BFRuntimeError Bool
  -- initializeState' :: Int -> String {- The input if the state debugs -} -> state

executeInstruction :: (BFState m state)
                    => state -> Instruction f -> ExceptT BFRuntimeError m state
executeInstruction state instruction 
  = case instruction of
      ModifyPointer val -> ExceptT $ Right <$> modifyPointerST val state
      ModifyCell    val -> ExceptT $ Right <$> modifyCellST val state
      GetChar    -> getCharST state
      PutChar    -> putCharST state
      Loop prog  -> loopST prog state

loopST :: (Foldable program, BFState m state)
       => program (Instruction program) -> state -> ExceptT BFRuntimeError m state
loopST  program pST =
  case currentCellIsZeroST pST of
  Left err -> except $ Left err
  Right cellIsZero -> if cellIsZero
                      then pure pST -- exits loop
                      else interpret pST program >>= loopST program

interpret :: (Foldable program, BFState m state)
          => state -> program (Instruction program) -> ExceptT BFRuntimeError m state
interpret = foldlM executeInstruction
