module Graphics.FRPUtils where

import Graphics.FRP

lastE :: Int -> a -> FRP (Event a) [a]
lastE n init = foldp newEvent inits
  where
    inits = replicate n init
    newEvent new before = take n $ new : before
