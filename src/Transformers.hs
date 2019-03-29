module Transformers where

import Control.Monad.Except
import qualified Control.Monad.Fail as F
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Maybe

type Name = String

data Exp
  = Lit Integer
  | Var Name
  | Plus Exp
         Exp
  | Abs Name
        Exp
  | App Exp
        Exp
  deriving (Show)

data Value
  = IntVal Integer
  | FunVal Env
           Name
           Exp
  deriving (Show)

type Env = Map.Map Name Value

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


-- Use monad
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


-- Add ExceptT monad transformer
type EVal2 a = ExceptT String Identity a

runEval2 :: EVal2 a -> Either String a
runEval2 = runIdentity . runExceptT

eval2 :: Env -> Exp -> EVal2 Value
eval2 _ (Lit i) = return $ IntVal i
eval2 env (Var x) = case Map.lookup x env of
  Just v -> return v
  Nothing -> throwError $ "unbound variable: " ++ show x
eval2 env (Plus exp1 exp2) =
  do
    r1 <- eval2 env exp1
    r2 <- eval2 env exp2
    case (r1, r2) of
      (IntVal a, IntVal b) -> return $ IntVal (a + b)
      e -> throwError $ "exception: " ++ show e
eval2 env (Abs name exp) = return $ FunVal env name exp
eval2 env (App exp1 exp2) =
  do
    r1 <- eval2 env exp1
    r2 <- eval2 env exp2
    case r1 of
      FunVal env' n body ->
        eval2 (Map.insert n r2 env') body
      _ -> throwError $ "type error in application"


-- Hide Env using Reader
type EVal3 a = ReaderT Env (ExceptT String Identity) a

runEval3 :: Env -> EVal3 a -> Either String a
runEval3 env e = runIdentity $ runExceptT $ runReaderT e env

eval3 :: Exp -> EVal3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var x) =
  do
    env <- ask
    case Map.lookup x env of
      Just v -> return v
      Nothing -> throwError $ "unbound variable: " ++ show x
eval3 (Plus exp1 exp2) =
  do
    r1 <- eval3 exp1
    r2 <- eval3 exp2
    case (r1, r2) of
      (IntVal a, IntVal b) -> return $ IntVal $ a + b
      e -> throwError $ "exception: " ++ show e
eval3 (Abs name exp) =
  do
    env <- ask
    return $ FunVal env name exp
eval3 (App exp1 exp2) =
  do
    r1 <- eval3 exp1
    r2 <- eval3 exp2
    case r1 of
      FunVal env' n body ->
        local (const $ Map.insert n r2 env') $ eval3 body
      _ -> throwError $ "type error in application"
