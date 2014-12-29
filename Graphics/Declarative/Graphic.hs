module Graphics.Declarative.Graphic where

type Offset = (Double, Double)
type MoveFunc backend = Offset -> backend -> backend
type AtopFunc backend = backend -> backend -> backend

newtype Graphic backend = Graphic (MoveFunc backend -> AtopFunc backend -> backend)

renderGraphic :: MoveFunc backend -> AtopFunc backend -> Graphic backend -> backend
renderGraphic move atop (Graphic renderer) = renderer move atop

primitive :: backend -> Graphic backend
primitive prim = Graphic $ \ move atop -> prim

move :: Offset -> Graphic backend -> Graphic backend
move offset graphic = Graphic $ \ move atop -> move offset (renderGraphic move atop graphic)

atop :: Graphic backend -> Graphic backend -> Graphic backend
atop upper lower    = Graphic $ \ move atop -> atop (renderGraphic move atop upper) (renderGraphic move atop lower)
