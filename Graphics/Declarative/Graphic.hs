module Graphics.Declarative.Graphic where

import Graphics.Declarative.Envelope

type RGB = (Double, Double, Double)

data Graphic backend
  = Leaf backend
  | Moved (Double, Double) (Graphic backend)
  | Atop (Graphic backend) (Graphic backend)
  | Empty

-- BUILDING Graphics:

moveGraphic :: (Double, Double) -> Graphic b -> Graphic b
moveGraphic (x, y) (Moved (px, py) g) = Moved (px+x, py+y) g
moveGraphic amount g = Moved amount g

atopGraphics :: Graphic b -> Graphic b -> Graphic b
atopGraphics = Atop
