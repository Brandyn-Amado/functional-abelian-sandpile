module Sandpile.Data where

import Data.Int (Int64)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

data Sandpile = Sandpile
  { size :: Int,
    step :: Int,
    terminated :: Bool,
    grid :: V.Vector (U.Vector Int64)
  }
  deriving (Show, Eq)