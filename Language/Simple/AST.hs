module Language.Simple.AST (
  SimpleExpr(..)
)
where

import Language.Simple.Values

type Name = String

data SimpleExpr = SEConst SimpleValue
                | SEVar Name
                | SEApp SimpleExpr SimpleExpr
                | SELet Name SimpleExpr SimpleExpr
                | SELambda Name SimpleExpr
                deriving Show
