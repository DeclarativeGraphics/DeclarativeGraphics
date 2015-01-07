module Graphics.Declarative.Border where

import qualified Data.Vec2 as Vec2
import Data.Vec2 (Vec2)

newtype Border = Border { borderDistance :: Vec2 -> Double }

borderOffset :: Border -> Vec2 -> Vec2
borderOffset border direction = Vec2.scale (borderDistance border direction) direction

borderSpanOnAxis :: Border -> Vec2 -> Vec2
borderSpanOnAxis border axis = back `Vec2.to` forth
  where
    forth = borderOffset border axis
    back  = borderOffset border (Vec2.negate axis)

getBoundingBox :: Border -> (Vec2, Vec2)
getBoundingBox border = (Vec2.add left top, Vec2.add right bottom)
  where
    left   = borderOffset border Vec2.left
    top    = borderOffset border Vec2.up
    right  = borderOffset border Vec2.right
    bottom = borderOffset border Vec2.down

size :: Border -> (Double, Double)
size (Border f) = (f Vec2.right + f Vec2.left, f Vec2.down + f Vec2.up)

tangentDistance :: Vec2 -> Vec2 -> Double
tangentDistance direction corner = direction `Vec2.dot` corner

empty :: Border
empty = Border (const 0)

circle :: Double -> Border
circle radius = Border $ \q -> radius / Vec2.magnitude q

-- relative origin, within rectangle
rectangle :: (Double, Double) -> Double -> Double -> Border
rectangle (xorigin, yorigin) width height
  = fromBoundingBox ((-width * xorigin,    -height * yorigin),
                     (width * (1-xorigin), height * (1-yorigin)))

fromBoundingBox :: (Vec2, Vec2) -> Border
fromBoundingBox ((l, t), (r, b)) = Border $
    \ direction -> maximum $ map (tangentDistance direction) corners
  where
    corners = [ (x,y) | x <- [l,r], y <- [t,b] ]

rotate :: Double -> Border -> Border
rotate angle (Border f) = Border $ \q -> f (Vec2.rotateBy (-angle) q)

scale :: (Double, Double) -> Border -> Border
scale factors (Border f) = Border $ \q -> f (Vec2.mul factors q)

move :: Vec2 -> Border -> Border
move v (Border f) = Border $ \q -> f q + (v `Vec2.dot` q) / (q `Vec2.dot` q) -- magic shit

atop :: Border -> Border -> Border
atop (Border f1) (Border f2) = Border $ \q -> max (f1 q) (f2 q)

padded :: Double -> Border -> Border
padded amount (Border f) = Border $ \q -> amount / Vec2.magnitude q + f q -- It's like adding a circle
