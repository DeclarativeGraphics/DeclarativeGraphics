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