module Graphics.Declarative.Border where

import Data.Vec2 as Vec2

newtype Border = Border { borderDistance :: Vec2 -> Double }

borderOffset :: Border -> Vec2 -> Vec2
borderOffset border direction = scale (borderDistance border direction) direction

borderSpanOnAxis :: Border -> Vec2 -> Vec2
borderSpanOnAxis border axis = back `Vec2.to` forth
  where
    forth = borderOffset border axis
    back  = borderOffset border (Vec2.negate axis)

empty :: Border
empty = Border (const 0)

getBoundingBox :: Border -> (Vec2, Vec2)
getBoundingBox border = (add left top, add right bottom)
  where
    left   = borderOffset border Vec2.left
    top    = borderOffset border Vec2.up
    right  = borderOffset border Vec2.right
    bottom = borderOffset border Vec2.down

fromBoundingBox :: (Vec2, Vec2) -> Border
fromBoundingBox ((l, t), (r, b)) = Border
    (\ direction -> maximum $ map (tangentDistance direction) corners)
  where
    corners = [ (x,y) | x <- [l,r], y <- [t,b] ]

tangentDistance :: Vec2 -> Vec2 -> Double
tangentDistance direction corner = direction `dot` corner

circle :: Double -> Border
circle radius = Border (const radius)

-- relative origin, within rectangle
rectangle :: (Double, Double) -> Double -> Double -> Border
rectangle (xorigin, yorigin) width height
  = fromBoundingBox ((-width * xorigin,    -height * yorigin),
                     (width * (1-xorigin), height * (1-yorigin)))

-- Be careful! This doesn't magically move the Graphical Representation of the Form with it!
move :: Vec2 -> Border -> Border
move v (Border f) = Border (\q -> f q + v `dot` q) -- magic shit

size :: Border -> (Double, Double)
size (Border f) = (f (1, 0) + f (-1, 0), f (0, 1) + f (0, -1))

atop :: Border -> Border -> Border
atop (Border f1) (Border f2) = Border (\q -> max (f1 q) (f2 q))

padded :: Double -> Border -> Border
padded amount (Border f)
  = Border (\v -> amount + f v)
