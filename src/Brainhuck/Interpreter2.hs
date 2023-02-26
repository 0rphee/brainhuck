module Brainhuck.Interpreter2 () where

import qualified Data.Vector.Unboxed.Mutable as VU
import qualified Data.Sequence as S
import Control.Monad.Primitive
import Control.Exception
import Data.Maybe (fromMaybe)
import Data.Foldable (foldlM)
-- import Brainhuck.Parsing (parseProgram)
-- import Brainhuck.Types
import Control.Monad (void)
import Data.Word (Word8)

-- =====================================================================
-- Types

type MemoryCell = Word8

type AltMem = VU.MVector (PrimState IO) MemoryCell

init' :: Int -> IO (VU.MVector (PrimState IO) Word)
init' memSize = VU.replicate memSize 0
