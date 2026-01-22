module Sandpile.Format where

import Sandpile.Data

import Data.Strings
import Data.Int (Int64)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U

-- Grid cell padding for printing to screen
cellSpacing :: Int
cellSpacing = 4

-- Convert one grid row into a string
rowToString :: U.Vector Int64 -> String
rowToString vec = strTrim (strJoin "" (map (strPadRight ' ' cellSpacing) (strSplitAll "," (strReplace "]" "" (strReplace "[" "" (Prelude.show vec))))))

-- Convert a grid into a formatted string
gridToString :: V.Vector (U.Vector Int64) -> String
gridToString g = strTrim (foldl (\acc row -> acc ++ "\n" ++ row) "" (V.map rowToString g))

-- Convert a sandpile's metadata to a foratted string
pileMetaToString :: Sandpile -> String
pileMetaToString pile =   
  let n = Prelude.show (size pile); stepNum = Prelude.show (step pile); term = Prelude.show (terminated pile)
   in "Sandpile {dimensions: " ++ n ++ " x " ++ n ++ ", step: " ++ stepNum ++ ", terminated: " ++ term ++ "}"


-- Convert a sandpile into a formatted string
pileToString :: Sandpile -> String
pileToString pile = pileMetaToString pile ++ "\n" ++ gridToString (grid pile) ++ "\n"
