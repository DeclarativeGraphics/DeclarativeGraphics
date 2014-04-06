module DrawTypes where

type Color = (Double, Double, Double)

data LineStyle = LineStyle {
  color :: Color,
  lineWidth :: Double,
  cap :: LineCap,
  join :: LineJoin,
  dash :: [Double],
  dashOffset :: Double
} deriving (Show, Eq)

defaultLineStyle :: LineStyle
defaultLineStyle = LineStyle {
  color = (0, 0, 0),
  lineWidth = 1,
  cap = Padded,
  join = Sharp,
  dash = [],
  dashOffset = 0
}

data LineCap = Flat | Round | Padded deriving (Show, Eq)
data LineJoin = Smooth | Sharp | Clipped deriving (Show, Eq)
