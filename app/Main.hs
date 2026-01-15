module Main where

import qualified Sandpile (initSandpile, stepSandpile)
import System.Environment ( getArgs )
import System.Exit ( die )
import Data.List.Split ( splitOn )
import Data.Int (Int64)

usage :: String
usage = "Usage: sandpile --size [size] --init-path [path]"

defaults :: (Int, String)
defaults = (16, "")

parseInitialState :: (Int, String) -> IO (Int, [(Int, Int, Int64)])
parseInitialState (size, "") = pure (size, [(size `div` 2, size `div` 2, (fromIntegral size) * (fromIntegral size))])
parseInitialState (size, path) = fmap (\contents -> (size, map (\line -> read ("(" ++ line ++ ")") :: (Int, Int, Int64)) (splitOn "\n" contents))) (readFile path)

parseArgs :: (Int, String) -> [String] -> IO (Int, String)
parseArgs args [] = pure args
parseArgs (_, path) ("--size":size:rest) = parseArgs (read size :: Int, path) rest
parseArgs (size, _) ("--init-path":path:rest) = parseArgs (size, path) rest
parseArgs _ _  = die usage

main :: IO ()
main = (getArgs >>= parseArgs defaults >>= parseInitialState) >>= print . Sandpile.initSandpile
