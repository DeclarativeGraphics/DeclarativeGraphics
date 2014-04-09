module Graphics.Declarative.Envelope where

data Envelope = Envelope {
  envToLeft :: Double,
  envToTop :: Double,
  envToRight :: Double,
  envToBottom :: Double
} deriving (Show, Eq)

envelopeCenteredRect :: Double -> Double -> Envelope
envelopeCenteredRect width height = Envelope (-width / 2) (-height / 2) (width / 2) (height / 2)

envelopeCenteredCircle :: Double -> Envelope
envelopeCenteredCircle radius = Envelope (-radius) (-radius) radius radius

moveEnvelope :: (Double, Double) -> Envelope -> Envelope
moveEnvelope (x, y) (Envelope l t r b) = Envelope (l+x) (t+y) (r+x) (b+y)

envelopeSize :: Envelope -> (Double, Double)
envelopeSize (Envelope l t r b) = (r-l, b-t)

envelopeAtop :: Envelope -> Envelope -> Envelope
envelopeAtop (Envelope l1 t1 r1 b1) (Envelope l2 t2 r2 b2) = Envelope (min l1 l2) (min t1 t2) (max r1 r2) (max b1 b2)
