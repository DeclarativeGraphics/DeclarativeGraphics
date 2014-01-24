module Language.Simple.Backends.Evaluator where

import Language.Simple.Backends.Evaluator.Prim
import Language.Simple.Backends.Evaluator.Values
import Language.Simple.Backends.Evaluator.Primitives

import Control.Monad

type Compilation = Evaluator SimpleValue

evalTop :: Compilation -> SimpleValue
evalTop c = evaluate c primArithmetic


compileConst :: Int -> Compilation
compileConst x = return $ SVNum x

compileVar :: Name -> Compilation
compileVar var = lookupEnv var

compileApp :: Compilation -> Compilation -> Compilation
compileApp f x = liftM2 apply f x
  where apply (SVFunc f) x = f x
        apply x _ = error $ "invalid function: " ++ show x

compileLet :: Name -> Compilation -> Compilation -> Compilation
compileLet var compiledval compiledbody = do
  val <- compiledval
  with (addEnv var val) compiledbody

compileLambda :: Name -> Compilation -> Compilation
compileLambda var body = liftM SVFunc $ closure compiledFunction
  where
    compiledFunction arg = with (addEnv var arg) body
