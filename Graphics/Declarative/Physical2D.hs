module Graphics.Declarative.Physical2D where

import qualified Data.Vec2 as Vec2
import Data.Vec2 (Vec2)

class Physical2D a where
  move :: (Double, Double) -> a -> a
  move = moveOrigin . Vec2.negate

  moveOrigin :: (Double, Double) -> a -> a
  moveOrigin = move . Vec2.negate

  rotate :: Double -> a -> a

  scale :: (Double, Double) -> a -> a

  atop :: a -> a -> a

  atopAll :: [a] -> a
  atopAll = foldl atop empty

  empty :: a
