module Graphics.Declarative.Shape where

import qualified Graphics.Declarative.Internal.Primitive as Prim
import Graphics.Declarative.Envelope

data Shape = Shape {
  sEnvelope :: Envelope,
  sPrims :: [Prim.Primitive]
} deriving (Show, Eq)

circle :: Double -> Shape
circle radius = Shape {
  sEnvelope = envelopeCenteredCircle radius,
  sPrims = [Prim.Arc 0 0 radius 0 (2 * pi)]
}

rectangle :: Double -> Double -> Shape
rectangle width height = Shape {
  sEnvelope = envelopeCenteredRect width height,
  sPrims = [Prim.Rectangle (-width/2) (-height/2) width height]
}

empty :: Shape
empty = blankCentered 0 0

blankCentered :: Double -> Double -> Shape
blankCentered w h = onlyEnvelope $ Envelope (-w/2) (-h/2) (w/2) (h/2)

onlyEnvelope :: Envelope -> Shape
onlyEnvelope env = Shape env []

fromEnvelope :: Envelope -> Shape
fromEnvelope env@(Envelope l t r b) = Shape {
  sEnvelope = env,
  sPrims = [Prim.Rectangle l t (r-l) (b-t)]
}
