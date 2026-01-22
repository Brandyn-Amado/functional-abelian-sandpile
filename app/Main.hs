module Main where

import Sandpile
import System.Environment ( getArgs )
import System.Exit ( die )
import Data.List.Split ( splitOn )
import Data.Int (Int64)

usage :: String
usage = "Usage: cabal run sandpile -- --size [size] --in-path [path] --progress-freq [steps] [-v]"

defaults :: (Int, String, Int, Bool)
defaults = (16, "", 25, False)

parseInitialState :: (Int, String, Int, Bool) -> IO (Int, [(Int, Int, Int64)], Int, Bool)
parseInitialState (size, "", steps, verbose) = pure (size, [(size `div` 2, size `div` 2, fromIntegral size * fromIntegral size)], steps, verbose)
parseInitialState (size, path, steps, verbose) = fmap (\contents -> (size, map (\line -> read ("(" ++ line ++ ")") :: (Int, Int, Int64)) (splitOn "\n" contents), steps, verbose)) (readFile path)

parseArgs :: (Int, String, Int, Bool) -> [String] -> IO (Int, String, Int, Bool)
parseArgs args [] = pure args
parseArgs (_, path, steps, verbose) ("--size":size:rest) = parseArgs (read size :: Int, path, steps, verbose) rest
parseArgs (size, _, steps, verbose) ("--in-path":path:rest) = parseArgs (size, path, steps, verbose) rest
parseArgs (size, path, _, verbose) ("--progress-freq":steps:rest) = parseArgs (size, path, read steps :: Int, verbose) rest
parseArgs (size, path, steps, _) ("-v":rest) = parseArgs (size, path, steps, True) rest
parseArgs _ _  = die usage

main :: IO ()
main = putStrLn "Simulation starting...\n" >>= (\() -> getArgs >>= parseArgs defaults >>= parseInitialState) >>= (\(size, initState, printFreq, verbose) -> (reportProgressUntilTermination verbose printFreq) (initSandpile (size, initState))) >>= (\_ -> putStrLn "Simulation complete!")