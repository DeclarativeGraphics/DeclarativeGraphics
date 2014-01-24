module Language.Simple.AST where

type Name = String

data SimpleExpr = SEConst Int
                | SEVar Name
                | SEApp SimpleExpr SimpleExpr
                | SELet Name SimpleExpr SimpleExpr
                | SELambda Name SimpleExpr
                deriving Show
