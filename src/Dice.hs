module Dice where

import System.Random
import Control.Applicative

rollDiceIO :: IO (Int, Int)
rollDiceIO = liftA2 (,) (randomRIO (1,6)) (randomRIO (1,6))

roll1DiceIO :: IO Int
roll1DiceIO =  randomRIO (1, 6)

rollNDiceIO :: Int -> IO [Int]
rollNDiceIO = sequence . (flip replicate) roll1DiceIO

roll1Dice :: StdGen -> (Int, StdGen)
roll1Dice = randomR (1, 6)

rollDice :: StdGen -> ((Int, Int), StdGen)
rollDice g = ((a, b), ng)
  where (a, g') = roll1Dice g
        (b, ng) = roll1Dice g'
