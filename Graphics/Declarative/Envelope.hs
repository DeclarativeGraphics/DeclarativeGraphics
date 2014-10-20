module Graphics.Declarative.Envelope where

data Envelope = Envelope {
  envToLeft :: Double,
  envToTop :: Double,
  envToRight :: Double,
  envToBottom :: Double
} deriving (Show, Eq)

emptyEnvelope :: Envelope
emptyEnvelope = Envelope 0 0 0 0

envelopeCenteredRect :: Double -> Double -> Envelope
envelopeCenteredRect width height = Envelope (-width / 2) (-height / 2) (width / 2) (height / 2)

envelopeCenteredCircle :: Double -> Envelope
envelopeCenteredCircle radius = Envelope (-radius) (-radius) radius radius

-- Be careful! This doesn't magically move the Graphical Representation of the Form with it!
moveEnvelope :: (Double, Double) -> Envelope -> Envelope
moveEnvelope (x, y) (Envelope l t r b) = Envelope (l+x) (t+y) (r+x) (b+y)

sizeEnvelope :: Envelope -> (Double, Double)
sizeEnvelope (Envelope l t r b) = (r-l, b-t)

atopEnvelope :: Envelope -> Envelope -> Envelope
atopEnvelope (Envelope l1 t1 r1 b1) (Envelope l2 t2 r2 b2) = Envelope (min l1 l2) (min t1 t2) (max r1 r2) (max b1 b2)

paddedWithEnvelope :: (Double, Double, Double, Double) -> Envelope -> Envelope
paddedWithEnvelope (padLeft, padTop, padRight, padBottom) (Envelope l t r b)= Envelope (l-padLeft) (t-padTop) (r+padRight) (b+padBottom)
