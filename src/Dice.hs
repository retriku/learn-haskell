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
  sf <*> a = State $ \s ->
    let (f, ns) = (runState sf) s
        b = f `fmap` a
    in (runState b) ns

instance Monad (State s) where
  return a = State(\s -> (a, s))
  s >>= c = State $ \s'->
    let (a, ns) = (runState s) s'
    in (runState $ c a) ns

put ns = State $ \_ -> ((), ns)
get = State $ \s -> (s, s)

evalState :: State s a -> s -> a
evalState p s = fst (runState p s)

execState :: State s a -> s -> s
execState p s = snd (runState p s)


push :: a -> State [a] ()
push a = do
  ns <- get
  put (a : ns)

pop :: State [a] (Maybe a)
pop = do
  os <- get
  case os of
    [] -> State $ \_ -> (Nothing, [])
    [x] -> State $ \_ -> (Just x, [])
    (x:xs) -> State $ \_ -> (Just x, xs)

work = do
  push 3
  push 2
  push 1
  pop
  push 4
  pop
  pop
  pop
