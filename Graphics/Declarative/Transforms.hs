{-# LANGUAGE FlexibleInstances #-}
module Graphics.Declarative.Transforms where

import Linear

class Transformable a where
  transformBy :: M33 Double -> a -> a

instance Transformable (V3 Double) where
  transformBy matrix vec = matrix !* vec

instance Transformable (V2 Double) where
  transformBy matrix (V2 x y) =
    let
      (V3 x' y' w) =
        matrix !* V3 x y 1
    in
    V2 (x' / w) (y' / w)

moveMatrix :: Num a => V2 a -> M33 a
moveMatrix (V2 offsetx offsety) =
  V3 (V3 1 0 offsetx)
     (V3 0 1 offsety)
     (V3 0 0       1)

scaleMatrix :: Num a => V2 a -> M33 a
scaleMatrix (V2 scalex scaley) =
  V3 (V3 scalex 0 0)
     (V3 0 scaley 0)
     (V3 0 0      1)

rotateRadMatrix :: Floating a => a -> M33 a
rotateRadMatrix r =
  V3 (V3 (cos r) (- sin r) 0)
     (V3 (sin r) (  cos r) 0)
     (V3      0         0  1)

move :: Transformable a => V2 Double -> a -> a
move = transformBy . moveMatrix

moveOrigin :: Transformable a => V2 Double -> a -> a
moveOrigin = move . ((-1) *^)

scale :: Transformable a => V2 Double -> a -> a
scale = transformBy . scaleMatrix

rotateRad :: Transformable a => Double -> a -> a
rotateRad = transformBy . rotateRadMatrix

transposeTransform :: Num a => M33 a -> V2 a -> V2 a
transposeTransform matrix (V2 x y) = toV2 (transpose matrix !* (V3 x y 0))

inverseTransform :: M33 Double -> V2 Double -> V2 Double
inverseTransform matrix (V2 x y) = toV2 (inv33 matrix !* (V3 x y 0))

translation :: M33 Double -> V2 Double
translation (V3 (V3 _ _ x) (V3 _ _ y) _) = V2 x y

toV2 :: V3 a -> V2 a
toV2 (V3 x y _) = V2 x y

left, right, up, down :: Num a => V2 a
left  = V2 (-1)  0
right = V2   1   0
up    = V2   0 (-1)
down  = V2   0   1
