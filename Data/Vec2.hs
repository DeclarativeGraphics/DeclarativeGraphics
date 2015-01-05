module Data.Vec2 where

type Vec2 = (Double, Double)

add :: Vec2 -> Vec2 -> Vec2
add (x1, y1) (x2, y2) = (x1+x2, y1+y2)

sub :: Vec2 -> Vec2 -> Vec2
sub (x1, y1) (x2, y2) = (x1-x2, y1-y2)

to :: Vec2 -> Vec2 -> Vec2
a `to` b = b `sub` a

zero :: Vec2
zero = (0,0)

negate :: Vec2 -> Vec2
negate v = zero `sub` v

magnitude :: Vec2 -> Double
magnitude (x, y) = sqrt (x*x + y*y)

normalize :: Vec2 -> Vec2
normalize v@(a, b) = (a / m, b / m) where m = magnitude v

scale :: Double -> Vec2 -> Vec2
scale factor (a, b) = (a*factor, b*factor)

dot :: Vec2 -> Vec2 -> Double
dot (a1, a2) (b1, b2) = a1*b1 + a2*b2

right, left, down, up :: Vec2
right = ( 1,  0)
left  = (-1,  0)
down  = ( 0,  1)
up    = ( 0, -1)

radians :: Double -> Double
radians = id

degrees :: Double -> Double
degrees = (*) (pi / 180)

rotateBy :: Double -> Vec2 -> Vec2
rotateBy rad (x, y) = (x * c - y * s, x * s + y * c)
  where
    c = cos rad
    s = sin rad
