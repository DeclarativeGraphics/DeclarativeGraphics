{-# LANGUAGE GADTs #-}
module Graphics.Graphics where

import Grpahics.Point

import Data.List

import Graphics.Rendering.Cairo

type Coord a = (a,a)
data RGB a = RGB a a a deriving Show

type Color = RGB Double

type Size = Double
type Radius = Double

data Shape where
  ClosedPath :: (HAxes h, VAxes v) => [Point Double h v] -> Shape
  OpenPath   :: (HAxes h, VAxes v) => [Point Double h v] -> Shape
  Circle     :: (HAxes h, VAxes v) => Double -> Point Double h v -> Shape

data StrokeOptions = DefaultStrokeOptions
                   deriving Show

dso = DefaultStrokeOptions

data Form where
  Filled :: Color -> Shape -> Form
  Outlined :: StrokeOptions ->  Color -> Shape -> Form
  AsForm :: (HAxes h, VAxes v) => Canvas -> Point Double h v -> Form

data Canvas = Canvas Double Double [Form]
            | Text Color Size String

addForm form (Canvas w h forms) = Canvas w h (form:forms)

canvasSize :: Canvas -> Coord Double
canvasSize (Canvas w h _) = (w,h)
canvasSize (Text _ size text) = textSize size text

textSize :: Double -> String -> Coord Double
textSize size text = (0.6 * size * genericLength text, size)

x |> f = f x
