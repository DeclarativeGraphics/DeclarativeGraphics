module VecMath where

type Vec2 = (Double, Double)

magnitude :: Vec2 -> Double
magnitude (x, y) = sqrt (x*x + y*y)

normalize :: Vec2 -> Vec2
normalize v@(a, b) = (a / m, b / m) where m = magnitude v

scale :: Vec2 -> Double -> Vec2
scale (a, b) factor = (a*factor, b*factor)

dot :: Vec2 -> Vec2 -> Double
dot (a1, a2) (b1, b2) = a1*b1 + a2*b2
