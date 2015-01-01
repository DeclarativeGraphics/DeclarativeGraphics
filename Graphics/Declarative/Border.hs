module Graphics.Declarative.Border where

import VecMath

newtype Border = Border { query :: (Double, Double) -> Double }

rightBorderOffset :: Border -> Double
rightBorderOffset (Border f) = f (1, 0)

leftBorderOffset :: Border -> Double
leftBorderOffset (Border f) = - f (-1, 0)

bottomBorderOffset :: Border -> Double
bottomBorderOffset (Border f) = f (0, 1)

topBorderOffset :: Border -> Double
topBorderOffset (Border f) = - f (0, -1)

empty :: Border
empty = Border (const 0)

fromFrame :: (Double, Double, Double, Double) -> Border
fromFrame (l, t, r, b) = Border
    (\ direction -> maximum $ map (tangentDistance direction) corners)
  where
    corners = [ (x,y) | x <- [l,r], y <- [t,b] ]

tangentDistance direction corner = direction `dot` corner

circular :: Double -> Border
circular radius = Border (const radius)

-- relative origin, within rectangular
rectangular :: (Double, Double) -> Double -> Double -> Border
rectangular (xorigin, yorigin) width height
  = fromFrame (-width * xorigin,    -height * yorigin,
               width * (1-xorigin), height * (1-yorigin))

-- Be careful! This doesn't magically move the Graphical Representation of the Form with it!
move :: (Double, Double) -> Border -> Border
move v (Border f) = Border (\q -> f q + v `dot` q) -- magic shit

size :: Border -> (Double, Double)
size (Border f) = (f (1, 0) + f (-1, 0), f (0, 1) + f (0, -1))

atop :: Border -> Border -> Border
atop (Border f1) (Border f2) = Border (\q -> max (f1 q) (f2 q))

padded :: Double -> Border -> Border
padded amount (Border f)
  = Border (\v -> amount + f v)
