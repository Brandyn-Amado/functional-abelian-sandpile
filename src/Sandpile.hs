module Sandpile (initSandpile, stepSandpile) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Int (Int64)

data SandpileMeta = SandpileMeta {
  size :: Int,
  step :: Int,
  isTerminated :: Bool
} deriving (Show)

data Sandpile = Sandpile {
  meta :: SandpileMeta,
  grid :: V.Vector (U.Vector Int64)
} deriving (Show)

zeroGrid :: (U.Unbox a, Num a) => Int -> V.Vector (U.Vector a)
zeroGrid n = V.generate n (\_ -> U.replicate n 0)

initSandpile :: (Int, [(Int, Int, Int64)]) -> Sandpile
initSandpile (dim, initState) = Sandpile{
  meta = SandpileMeta{size = dim, step = 0, isTerminated = False},
  grid = foldl (\arr (i, j, v) -> arr V.// [(i, (arr V.! i) U.// [(j, v)])]) (zeroGrid dim) initState
}

stepSandpile :: IO ()
stepSandpile = putStrLn "Not yet implemented"