module Sandpile where

import Sandpile.Format
import Sandpile.Utils
import Sandpile.Data

import Data.Int (Int64)

getSize :: Sandpile -> Int
getSize = size

getStep :: Sandpile -> Int
getStep = step

isTerminated :: Sandpile -> Bool
isTerminated = terminated

printSandpile :: Bool -> Sandpile -> IO ()
printSandpile verbose = putStrLn . (if verbose then pileToString else pileMetaToString)

-- Sandpile of size dim with a grid of values in the initState list
-- If a cell (i, j) is not preset in the list it initializes to zero
initSandpile :: (Int, [(Int, Int, Int64)]) -> Sandpile
initSandpile (dim, initState) = foldl withNewCellValue (zeroSandpile dim) initState

-- Progress one step in the sandpile simulation
stepSandpile :: Sandpile -> Sandpile
stepSandpile pile =
  if isTerminated pile
    then pile
    else
      let n = size pile
       in checkTermination (incrementStep (foldl stepCell pile [(i, j) | i <- [0 .. n - 1], j <- [0 .. n - 1]]))

-- Progress n steps in the sandpile simulation
stepN :: (Eq t, Num t) => Sandpile -> t -> Sandpile
stepN pile 1 = stepSandpile pile
stepN pile n = stepN (stepSandpile pile) (n - 1)

-- Progress until simulation terminates
stepUntilTermination :: Sandpile -> Sandpile
stepUntilTermination pile =
  if isTerminated pile
    then pile
    else stepUntilTermination (stepSandpile pile)

-- Progress until simulation terminates and print periodic progress updates
reportProgressUntilTermination :: Bool -> Int -> Sandpile -> IO Sandpile
reportProgressUntilTermination verbose logFreq pile =
  let {stepHelper logFreq pile n
         | isTerminated pile = fmap (\() -> pile) (printSandpile verbose pile)
         | n == 0 = printSandpile verbose pile >>= (\() -> stepHelper logFreq (stepSandpile pile) logFreq)
         | otherwise = stepHelper logFreq (stepSandpile pile) (n - 1)}
  in stepHelper logFreq pile logFreq