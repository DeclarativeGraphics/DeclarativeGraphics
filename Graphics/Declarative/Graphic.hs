module Graphics.Declarative.Graphic where

import Graphics.Declarative.Physical2D

type Offset = (Double, Double)
type Angle = Double
type RotateFunc backend = Angle -> backend -> backend
type ScaleFunc backend = (Double, Double) -> backend -> backend
type MoveFunc backend = Offset -> backend -> backend
type AtopFunc backend = backend -> backend -> backend

newtype Graphic backend = Graphic (RotateFunc backend -> ScaleFunc backend -> MoveFunc backend -> AtopFunc backend -> backend -> backend)

renderGraphic :: RotateFunc backend -> ScaleFunc backend -> MoveFunc backend -> AtopFunc backend -> backend -> Graphic backend -> backend
renderGraphic rotate scale move atop empty (Graphic renderer) = renderer rotate scale move atop empty

primitive :: backend -> Graphic backend
primitive prim = Graphic $ \ rotate scale move atop empty -> prim

instance Physical2D (Graphic backend) where
  rotate angle graphic  = Graphic $ \ rotate scale move atop empty -> rotate angle (renderGraphic rotate scale move atop empty graphic)
  scale factors graphic = Graphic $ \ rotate scale move atop empty -> scale factors (renderGraphic rotate scale move atop empty graphic)
  move offset graphic   = Graphic $ \ rotate scale move atop empty -> move offset (renderGraphic rotate scale move atop empty graphic)
  atop upper lower      = Graphic $ \ rotate scale move atop empty -> atop (renderGraphic rotate scale move atop empty upper) (renderGraphic rotate scale move atop empty lower)
  empty                 = Graphic $ \ rotate scale move atop empty -> empty
