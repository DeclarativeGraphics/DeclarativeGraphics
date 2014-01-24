module Language.Simple.Backends.Evaluator.Values where

type Func = SimpleValue -> SimpleValue

data SimpleValue = SVNum Int
                 | SVFunc Func

instance Show SimpleValue where
  show (SVNum num) = show num
  show (SVFunc _)  = "<func>"
