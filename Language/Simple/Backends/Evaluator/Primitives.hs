module Language.Simple.Backends.Evaluator.Primitives where

import Language.Simple.Values

svFunc2 f = SVFunc (\x -> SVFunc (\y -> f x y))

primArithmetic = map (onSnd primArithmeticFunc) [
  ("+", (+)),
  ("-", (-)),
  ("*", (*)),
  ("div", div)
  ]
  where
    onSnd f (x,y) = (x, f y)
    primArithmeticFunc func
      = svFunc2 $ \(SVNum x) (SVNum y) -> SVNum (func x y)
