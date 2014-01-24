module Language.Simple.Compiler where

import Language.Simple.AST

import Language.Simple.Backends.Evaluator


compile :: SimpleExpr -> Compilation
compile (SEConst x) = compileConst x
compile (SEVar var) = compileVar var
compile (SEApp f x) = compileApp (compile f) (compile x)
compile (SELet var valexpr expr)
                    = compileLet var (compile valexpr) (compile expr)
compile (SELambda var expr) = compileLambda var (compile expr)
