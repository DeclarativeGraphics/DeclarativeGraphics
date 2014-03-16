module Language.Simple.Values (
  SimpleValue(..)
)
where

type Func = SimpleValue -> SimpleValue

data SimpleValue = SVNum Int
                 | SVString String
                 | SVFunc Func

instance Show SimpleValue where
  show (SVNum num)  = show num
  show (SVString s) = show s
  show (SVFunc _)   = "<func>"
