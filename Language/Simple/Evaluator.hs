module Language.Simple.Evaluator where

import Language.Simple.Backends.Evaluator.Prim
import Language.Simple.Backends.Evaluator.Values
import Language.Simple.Backends.Evaluator.Primitives

type Compilation = Evaluate SimpleValue


compileConst :: Int -> Compilation
compileConst x = ret $ SVNum x

compileVar :: Name -> Compilation
compileVar var = lookupEnv var

compileApp :: Compilation -> Compilation -> Compilation
compileApp f x = on2 ($) f x

compileLet :: Name -> Compilation -> Compilation -> Compilation
compileLet var compiledval compiledbody = do
  val <- compiledval
  with (addEnv var val) compiledbody

compileLambda :: Name -> Compilation -> Compilation
compileLambda var body = liftM SVFunc $ closure compiledFunction
  where
    compiledFunction arg = with (addEnv var arg) body
