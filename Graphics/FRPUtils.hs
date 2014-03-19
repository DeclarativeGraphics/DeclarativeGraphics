module Graphics.FRPUtils where

import Graphics.FRP

runList network xs = tail $ map fst $ scanl (run . snd) (undefined, network) xs

lastEvents :: Int -> a -> FRP (Event a) (Behavior [a])
lastEvents n init = foldp newEvent inits
  where
    inits = replicate n init
    newEvent new before = take n $ new : before

hold :: a -> FRP (Event a) (Behavior a)
hold init = foldp takeNewValue init
  where takeNewValue newValue oldValue = newValue
