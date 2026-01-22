module Main where

import Sandpile
import System.Environment ( getArgs )
import System.Exit ( die )
import Data.List.Split ( splitOn )
import Data.Int (Int64)

usage :: String
usage = "Usage: cabal run sandpile -- --size [size] --in-path [path] --progress-freq [steps]"

defaults :: (Int, String, Int)
defaults = (16, "", 25)

parseInitialState :: (Int, String, Int) -> IO (Int, [(Int, Int, Int64)], Int)
parseInitialState (size, "", steps) = pure (size, [(size `div` 2, size `div` 2, fromIntegral size * fromIntegral size)], steps)
parseInitialState (size, path, steps) = fmap (\contents -> (size, map (\line -> read ("(" ++ line ++ ")") :: (Int, Int, Int64)) (splitOn "\n" contents), steps)) (readFile path)

parseArgs :: (Int, String, Int) -> [String] -> IO (Int, String, Int)
parseArgs args [] = pure args
parseArgs (_, path, steps) ("--size":size:rest) = parseArgs (read size :: Int, path, steps) rest
parseArgs (size, _, steps) ("--in-path":path:rest) = parseArgs (size, path, steps) rest
parseArgs (size, path, _) ("--progress-freq":steps:rest) = parseArgs (size, path, read steps :: Int) rest
parseArgs _ _  = die usage

main :: IO ()
main = putStrLn "Simulation starting...\n" >>= (\() -> getArgs >>= parseArgs defaults >>= parseInitialState) >>= (\(size, initState, printFreq) -> (verboseStepUntilTermination printFreq) (initSandpile (size, initState))) >>= (\_ -> putStrLn "\nSimulation complete!")