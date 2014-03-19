module Graphics.FRPUtils where

import Graphics.FRP

runList network xs = tail $ map fst $ scanl (run . snd) (undefined, network) xs

lastEvents :: Int -> [a] -> FRP (Event a) (Behavior [a])
lastEvents n init = foldp newEvent init
  where
    newEvent new before = take n $ new : before

maybeLastEvents :: Int -> FRP (Event a) (Behavior [Maybe a])
maybeLastEvents n = mapEvent Just >>> lastEvents n (replicate n Nothing)

hold :: a -> FRP (Event a) (Behavior a)
hold init = foldp takeNewValue init
  where takeNewValue newValue oldValue = newValue
