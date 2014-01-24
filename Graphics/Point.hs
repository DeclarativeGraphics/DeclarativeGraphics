{-# LANGUAGE GeneralizedNewtypeDeriving, NoMonomorphismRestriction #-}
module Graphics.Point where

data Point d x y = Point (x d) (y d) deriving Show

class HAxes a where
  toLeftToRight   :: Num d => d -> a d -> d
  fromLeftToRight :: Num d => d -> d -> a d
  hCoord :: d -> a d

newtype LeftToRight a = LeftToRight a deriving (Show, Num)
instance HAxes LeftToRight where
  toLeftToRight   width (LeftToRight x) = x
  fromLeftToRight width x               = LeftToRight x
  hCoord = LeftToRight

newtype RightToLeft a = RightToLeft a deriving (Show, Num)
instance HAxes RightToLeft where
  toLeftToRight   width (RightToLeft x) = width - x
  fromLeftToRight width x               = RightToLeft (width - x)
  hCoord = RightToLeft



class VAxes a where
  toTopDown   :: Num d => d -> a d -> d
  fromTopDown :: Num d => d -> d -> a d
  vCoord :: d -> a d

newtype TopDown a = TopDown a deriving (Show, Num)
instance VAxes TopDown where
  toTopDown   height (TopDown y) = y
  fromTopDown height y           = TopDown y
  vCoord = TopDown

newtype BottomUp a = BottomUp a deriving (Show, Num)
instance VAxes BottomUp where
  toTopDown   height (BottomUp y) = height - y
  fromTopDown height y            = BottomUp (height - y)
  vCoord = BottomUp




convertHAxes width  = fromLeftToRight width . toLeftToRight width
convertVAxes height = fromTopDown height . toTopDown height

convertPoint width height (Point x y)
  = Point (convertHAxes width x) (convertVAxes height y)


toCoords w h (Point x y) = (toLeftToRight w x, toTopDown h y)
fromCoords w h (x, y) = Point (fromLeftToRight w x) (fromTopDown h y)
onCoords w h f = uncurry f . toCoords w h

coords x y = Point (hCoord x) (vCoord y)


between fraction (Point xStart yStart) (Point xEnd yEnd)
  = Point (xStart + (xEnd - xStart) * hCoord fraction)
          (yStart + (yEnd - yStart) * vCoord fraction)

middle = between 0.5

topLeft     (Point top left) (Point bottom right) = Point top left
topRight    (Point top left) (Point bottom right) = Point top right
bottomLeft  (Point top left) (Point bottom right) = Point bottom left
bottomRight (Point top left) (Point bottom right) = Point bottom right

(Point x y) `relTo` (Point refX refY)
  = Point (x + refX) (y + refY)
