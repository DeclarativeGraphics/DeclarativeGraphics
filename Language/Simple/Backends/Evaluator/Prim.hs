module Language.Simple.Backends.Evaluator.Prim where

import Language.Simple.Values

import Data.List
import Data.Maybe


----- EVALUATOR MONAD -----

newtype Evaluator a = Eval (Env -> a)

evaluate :: Evaluator a -> Env -> a
evaluate (Eval e) = e

instance Monad Evaluator where
  return a = Eval (const a)
  e >>= f = Eval $ \env -> evaluate (f $ evaluate e env) env



----- ENVIRONMENT -----

type Name = String

type Env = [(Name, SimpleValue)]

addEnv var val env = (var,val):env
lookupEnv var = onEnv (fromMaybe unboundError . lookup var)
  where unboundError = error $ "unbound variable " ++ var




----- EVALUATOR PRIMITIVES -----

with :: (Env -> Env) -> Evaluator a -> Evaluator a
with alterEnv e = Eval $ evaluate e . alterEnv

closure :: (a -> Evaluator b) -> Evaluator (a -> b)
closure f = Eval (closed f)
  where
    closed f env arg = evaluate (f arg) env

onEnv :: (Env -> a) -> Evaluator a
onEnv f = Eval f
