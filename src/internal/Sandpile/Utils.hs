module Sandpile.Utils where

import Sandpile.Data

import Data.Int (Int64)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-- n x n grid with 0 at all cells
zeroGrid :: Int -> V.Vector (U.Vector Int64)
zeroGrid n = V.generate n (\_ -> U.replicate n 0)

-- Sandpile on step 0 with a zero grid
zeroSandpile :: Int -> Sandpile
zeroSandpile dim =
  Sandpile
    { size = dim,
      step = 0,
      terminated = False,
      grid = zeroGrid dim
    }

-- Returns true if (i, j) lies within the Sandpile's grid
inBounds :: Sandpile -> (Int, Int) -> Bool
inBounds pile (i, j) =
  let n = size pile
   in 0 <= i && i < n && 0 <= j && j < n

-- Returns a new sandpile with index (i, j) updated to value v
withNewCellValue :: Sandpile -> (Int, Int, Int64) -> Sandpile
withNewCellValue pile (i, j, v) =
  if inBounds pile (i, j)
    then
      let g = grid pile
       in pile {grid = g V.// [(i, (g V.! i) U.// [(j, v)])]}
    else pile

-- Read value of a sandpile at index (i, j)
(@) :: Sandpile -> (Int, Int) -> Maybe Int64
(@) pile (i, j) =
  if inBounds pile (i, j)
    then Just (grid pile V.! i U.! j)
    else Nothing

-- Returns a new sandpile with index (i, j) incremented by x
(+=) :: Sandpile -> (Int, Int, Int64) -> Sandpile
(+=) pile (i, j, x) =
  let maybe = pile @ (i, j)
   in case maybe of
        Just v -> withNewCellValue pile (i, j, v + x)
        Nothing -> pile

-- Increment step counter by 1
incrementStep :: Sandpile -> Sandpile
incrementStep pile = pile {step = step pile + 1}

-- If cell (i, j) has value v >= 4, then v is subtracted by 4
-- and its 4 cartesian neighbors increment by one
stepCell :: Sandpile -> (Int, Int) -> Sandpile
stepCell pile (i, j) =
  let maybe = pile @ (i, j)
   in case maybe of
        Just v ->
          if v >= 4
            then pile += (i, j, -4) += (i - 1, j, 1) += (i + 1, j, 1) += (i, j - 1, 1) += (i, j + 1, 1)
            else pile
        Nothing -> pile

-- Checks if the simulation should terminate and, if so, sets terminated to True
checkTermination :: Sandpile -> Sandpile
checkTermination pile
  | terminated pile = pile
  | V.foldl (&&) True (V.map (U.foldl (\b v -> v < 4 && b) True) (grid pile)) = pile {terminated = True}
  | otherwise = pile