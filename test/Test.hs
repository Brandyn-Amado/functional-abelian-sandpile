module Main (main) where

import Sandpile
import Sandpile.Utils
import Sandpile.Data

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Test.HUnit

testEqual :: (Eq a, Show a) => a -> a -> Test
testEqual actual expected = TestCase (assertEqual "" actual expected)

zeroPile :: Sandpile
zeroPile = zeroSandpile 2

testPile :: Sandpile
testPile = initSandpile (3, [(1, 1, 5), (0, 1, 1), (1, 0, 1), (1, 2, 1), (2, 1, 1)])

simulationPile :: Sandpile
simulationPile = initSandpile (4, [(2, 2, 16)])

testZeroGridEmpty :: Test
testZeroGridEmpty = testEqual (zeroGrid 0) (V.fromList [])

testZeroGridNonEmpty :: Test
testZeroGridNonEmpty = testEqual (zeroGrid 2) (V.fromList [U.fromList [0, 0], U.fromList [0, 0]])

testZeroSandpileEmpty :: Test
testZeroSandpileEmpty = testEqual (zeroSandpile 0) Sandpile {size = 0, step = 0, terminated = False, grid = zeroGrid 0}

testZeroSandpileNonEmpty :: Test
testZeroSandpileNonEmpty = testEqual zeroPile Sandpile {size = 2, step = 0, terminated = False, grid = zeroGrid 2}

inBoundsFalse :: Test
inBoundsFalse = testEqual (inBounds zeroPile (0, 2)) False

inBoundsTrue :: Test
inBoundsTrue = testEqual (inBounds zeroPile (0, 1)) True

testWithNewCellValueOutOfBounds :: Test
testWithNewCellValueOutOfBounds = testEqual (withNewCellValue zeroPile (4, 4, 16)) Sandpile {size = 2, step = 0, terminated = False, grid = zeroGrid 2}

testWithNewCellValueInBounds :: Test
testWithNewCellValueInBounds = testEqual (withNewCellValue zeroPile (0, 1, 16)) Sandpile {size = 2, step = 0, terminated = False, grid = V.fromList [U.fromList [0, 16], U.fromList [0, 0]]}

testAtOutOfBounds :: Test
testAtOutOfBounds = testEqual (zeroPile @ (0, 2)) Nothing

testAtInBounds :: Test
testAtInBounds = testEqual (withNewCellValue zeroPile (0, 1, 16) @ (0, 1)) (Just 16)

testPlusEqualOutOfBounds :: Test
testPlusEqualOutOfBounds = testEqual (zeroPile += (0, 2, 4)) zeroPile

testPlusEqualInBOunds :: Test
testPlusEqualInBOunds = testEqual (zeroPile += (0, 2, 4) += (0, 2, 3)) (withNewCellValue zeroPile (0, 2, 7))

testIncrementStep :: Test
testIncrementStep = testEqual (incrementStep (incrementStep zeroPile)) zeroPile {step = 2}

testCheckTerminationAlreadyTerminated :: Test
testCheckTerminationAlreadyTerminated = testEqual (checkTermination zeroPile {terminated = True}) zeroPile {terminated = True}

testCheckTerminationNotTerminated :: Test
testCheckTerminationNotTerminated = testEqual (checkTermination (zeroPile += (0, 0, 4))) (zeroPile += (0, 0, 4))

testCheckTerminationTerminate :: Test
testCheckTerminationTerminate =
  let pile = zeroPile += (0, 0, 3) += (0, 1, 3) += (1, 0, 3) += (1, 1, 3)
   in testEqual (checkTermination pile) pile {terminated = True}

testInitSandpileEmpty :: Test
testInitSandpileEmpty = testEqual (initSandpile (0, [])) Sandpile {size = 0, step = 0, terminated = False, grid = V.fromList []}

testInitSandpileZero :: Test
testInitSandpileZero = testEqual (initSandpile (4, [])) (zeroSandpile 4)

testInitSandpileNonZero :: Test
testInitSandpileNonZero = testEqual (initSandpile (4, [(0, 1, 16), (2, 2, 4), (3, 2, 8)])) (zeroSandpile 4 += (0, 1, 16) += (2, 2, 4) += (3, 2, 8))

testInitSandpileOutOfBounds :: Test
testInitSandpileOutOfBounds = testEqual (initSandpile (4, [(0, 4, 16), (-1, 0, 8), (0, 0, 2)])) (zeroSandpile 4 += (0, 0, 2))

testStepCellOutOfBounds :: Test
testStepCellOutOfBounds = testEqual (stepCell testPile (0, 3)) testPile

testStepCellNoChange :: Test
testStepCellNoChange = testEqual (stepCell (stepCell testPile (0, 0)) (0, 1)) testPile

testStepCellChange :: Test
testStepCellChange = testEqual (stepCell testPile (1, 1)) testPile {grid = V.fromList [U.fromList [0, 2, 0], U.fromList [2, 1, 2], U.fromList [0, 2, 0]]}

testStepCellFromEdge :: Test
testStepCellFromEdge = testEqual (stepCell (initSandpile (3, [(0, 1, 4)])) (0, 1)) (initSandpile (3, [(0, 0, 1), (0, 2, 1), (1, 1, 1)]))

testStepCellFromCorner :: Test
testStepCellFromCorner = testEqual (stepCell (initSandpile (3, [(0, 0, 4)])) (0, 0)) (initSandpile (3, [(0, 1, 1), (1, 0, 1)]))

testStepSandpile :: Test
testStepSandpile = testEqual (stepSandpile simulationPile) Sandpile {step = 1, size = 4, terminated = False, grid = V.fromList [U.fromList [0, 0, 0, 0], U.fromList [0, 0, 1, 0], U.fromList [0, 1, 12, 1], U.fromList [0, 0, 1, 0]]}

testStepNTwoSteps :: Test
testStepNTwoSteps = testEqual (stepN simulationPile 2) Sandpile {step = 2, size = 4, terminated = False, grid = V.fromList [U.fromList [0, 0, 0, 0], U.fromList [0, 0, 2, 0], U.fromList [0, 2, 8, 2], U.fromList [0, 0, 2, 0]]}

testStepNThreeSteps :: Test
testStepNThreeSteps = testEqual (stepN simulationPile 3) Sandpile {step = 3, size = 4, terminated = False, grid = V.fromList [U.fromList [0, 0, 0, 0], U.fromList [0, 0, 3, 0], U.fromList [0, 3, 4, 3], U.fromList [0, 0, 3, 0]]}

testStepUntilTermination :: Test
testStepUntilTermination = testEqual (stepUntilTermination simulationPile) Sandpile {step = 5, size = 4, terminated = True, grid = V.fromList [U.fromList [0, 0, 1, 0], U.fromList [0, 2, 1, 2], U.fromList [1, 1, 0, 1], U.fromList [0, 2, 1, 2]]}

tests :: Test
tests =
  TestList
    [ TestLabel "zeroGrid - empty" testZeroGridEmpty,
      TestLabel "zeroGrid - non-empty" testZeroGridNonEmpty,
      TestLabel "zeroSandpile - empty" testZeroSandpileEmpty,
      TestLabel "zeroSandpile - non-empty" testZeroSandpileNonEmpty,
      TestLabel "inBounds - False" inBoundsFalse,
      TestLabel "inBounds - True" inBoundsTrue,
      TestLabel "withNewCellValue - out of bounds index" testWithNewCellValueOutOfBounds,
      TestLabel "withNewCellValue - in bounds index" testWithNewCellValueInBounds,
      TestLabel "(@) operator - out of bounds index" testAtOutOfBounds,
      TestLabel "(@) operator - in bounds index" testAtInBounds,
      TestLabel "(+=) operator - out of bounds index" testPlusEqualOutOfBounds,
      TestLabel "(+=) operator - in bounds index" testPlusEqualInBOunds,
      TestLabel "incrementStep - only case" testIncrementStep,
      TestLabel "checkTermination - simulation already terminated" testCheckTerminationAlreadyTerminated,
      TestLabel "checkTermination - simulation still going" testCheckTerminationNotTerminated,
      TestLabel "checkTermination - simulation ready to terminate" testCheckTerminationTerminate,
      TestLabel "initSandpile - empty sandpile" testInitSandpileEmpty,
      TestLabel "initSandpile - zero sandpile" testInitSandpileZero,
      TestLabel "initSandpile - non-zero sandpile" testInitSandpileNonZero,
      TestLabel "initSandpile - ignore out of bounds" testInitSandpileOutOfBounds,
      TestLabel "stepCell - index out of bounds" testStepCellOutOfBounds,
      TestLabel "stepCell - value at index less than 4" testStepCellNoChange,
      TestLabel "stepCell - spill sand from inside of grid" testStepCellChange,
      TestLabel "stepCell - spill sand from edge of grid" testStepCellFromEdge,
      TestLabel "stepCell - spill sand pile corner of grid" testStepCellFromCorner,
      TestLabel "stepSandpile - one step with pile in center" testStepSandpile,
      TestLabel "stepN - two steps" testStepNTwoSteps,
      TestLabel "stepN - three steps" testStepNThreeSteps,
      TestLabel "stepUntilTermination" testStepUntilTermination 
    ]

main :: IO ()
main = runTestTT tests >>= print
