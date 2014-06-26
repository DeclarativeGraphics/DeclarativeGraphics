module FRP.Arrow where

import Prelude hiding ((.),id)
import Control.Category
import Control.Arrow

import FRP.Base


instance Category FRP where
  id = arr id
  (.) = flip andThen


instance Arrow FRP where
  arr = liftFRP
  first frp = parallel frp id
  second frp = parallel id frp
  (***) = parallel
  (&&&) = forkMerge (,)


{-
instance ArrowLoop FRP where
  loop aut = Step (\ input -> let (newAut,(outAut,state)) = step aut (input,state)
                              in (loop newAut, outAut))
-}
