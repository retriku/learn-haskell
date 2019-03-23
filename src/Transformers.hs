module Transformers where

import Control.Monad.Except
import qualified Control.Monad.Fail as F
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Maybe

type Name = String -- variable names

data Exp
  = Lit Integer -- expressions
  | Var Name
  | Plus Exp
         Exp
  | Abs Name
        Exp
  | App Exp
        Exp
  deriving (Show)

data Value
  = IntVal Integer -- values
  | FunVal Env
           Name
           Exp
  deriving (Show)

type Env = Map.Map Name Value -- mapping from names to values

eval0 :: Env -> Exp -> Value
eval0 _ (Lit i) = IntVal i
eval0 env (Var x) = fromJust $ Map.lookup x env
eval0 env (Plus exp1 exp2) = IntVal $ a + b
  where IntVal a = eval0 env exp1
        IntVal b = eval0 env exp2
eval0 env (Abs name exp) = FunVal env name exp
eval0  env(App e1  e2) = let val1=eval0  env  e1
                             val2=eval0  env  e2
                         in case val1 of FunVal env' n body -> eval0(Map.insert n val2  env')body

exampleExp = Lit 12 `Plus` (App (Abs "x" (Var "x")) (Lit 4 `Plus` Lit 2))

exampleExpFail = Lit 12 `Plus` (App (Abs "x" (Var "y")) (Lit 2))

eval1 :: F.MonadFail m => Env -> Exp -> m Value
eval1 _ (Lit i) = return $ IntVal i
eval1 env (Var x) =
  case Map.lookup x env of
    Just v -> return v
    Nothing -> F.fail "in your face"
eval1 env (Plus exp1 exp2) =
  do
    IntVal a <- eval1 env exp1
    IntVal b <- eval1 env exp2
    return $ IntVal (a + b)
eval1 env (Abs name exp) = return $ FunVal env name exp
eval1 env (App e1 e2) =
  do
    val1 <- eval1  env  e1
    val2 <- eval1  env  e2
    case val1 of
      FunVal env' n body ->
        eval1(Map.insert n val2  env')body
      _ -> F.fail "in your face 2"
