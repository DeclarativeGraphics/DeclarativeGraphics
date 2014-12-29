module Graphics.Declarative.Frame where

data Frame = Frame {
  leftBorderOffset, topBorderOffset, rightBorderOffset, bottomBorderOffset :: Double
} deriving (Show, Eq)

empty :: Frame
empty = Frame 0 0 0 0

-- relative origin, within frame
frame :: (Double, Double) -> Double -> Double -> Frame
frame (xorigin, yorigin) width height
  = Frame (-width * xorigin)    (-height * yorigin)
          (width * (1-xorigin)) (height * (1-yorigin))

-- Be careful! This doesn't magically move the Graphical Representation of the Form with it!
move :: (Double, Double) -> Frame -> Frame
move (x, y) (Frame l t r b) = Frame (l+x) (t+y) (r+x) (b+y)

size :: Frame -> (Double, Double)
size (Frame l t r b) = (r-l, b-t)

atop :: Frame -> Frame -> Frame
atop (Frame l1 t1 r1 b1) (Frame l2 t2 r2 b2) = Frame (min l1 l2) (min t1 t2) (max r1 r2) (max b1 b2)

padded :: (Double, Double, Double, Double) -> Frame -> Frame
padded (padLeft, padTop, padRight, padBottom) (Frame l t r b)
  = Frame (l-padLeft) (t-padTop) (r+padRight) (b+padBottom)
