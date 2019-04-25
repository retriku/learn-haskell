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

newtype State s a = State { runState :: s -> (a, s) }

state :: (s -> (a, s)) -> State s a
state = State

instance Functor (State s) where
  f `fmap` s = State(
    \s'->
      let (a, ns) = (runState s) s'
          b = f a
      in (b, ns)
    )

instance Applicative (State s) where
  pure a = State(\s -> (a, s))
  sf <*> a = State(
    \s ->
      let (f, ns) = (runState sf) s
          b = f `fmap` a
      in (runState b) ns
    )
