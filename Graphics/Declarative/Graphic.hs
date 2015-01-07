module Graphics.Declarative.Graphic where

type Offset = (Double, Double)
type Angle = Double
type RotateFunc backend = Angle -> backend -> backend
type ScaleFunc backend = (Double, Double) -> backend -> backend
type MoveFunc backend = Offset -> backend -> backend
type AtopFunc backend = backend -> backend -> backend

newtype Graphic backend = Graphic (RotateFunc backend -> ScaleFunc backend -> MoveFunc backend -> AtopFunc backend -> backend)

renderGraphic :: RotateFunc backend -> ScaleFunc backend -> MoveFunc backend -> AtopFunc backend -> Graphic backend -> backend
renderGraphic rotate scale move atop (Graphic renderer) = renderer rotate scale move atop

primitive :: backend -> Graphic backend
primitive prim = Graphic $ \ rotate scale move atop -> prim

rotate :: Angle -> Graphic backend -> Graphic backend
rotate angle graphic = Graphic $ \ rotate scale move atop -> rotate angle (renderGraphic rotate scale move atop graphic)

scale :: (Double, Double) -> Graphic backend -> Graphic backend
scale factors graphic = Graphic $ \ rotate scale move atop -> scale factors (renderGraphic rotate scale move atop graphic)

move :: Offset -> Graphic backend -> Graphic backend
move offset graphic = Graphic $ \ rotate scale move atop -> move offset (renderGraphic rotate scale move atop graphic)

atop :: Graphic backend -> Graphic backend -> Graphic backend
atop upper lower = Graphic $ \ rotate scale move atop -> atop (renderGraphic rotate scale move atop upper) (renderGraphic rotate scale move atop lower)
