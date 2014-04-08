module Primitive where

import qualified Graphics.Rendering.Cairo as Cairo
import DrawTypes

data Primitive 
  = Arc { xc :: Double, yc :: Double, radius :: Double, startAngle :: Double, endAngle :: Double }
  | ArcNeg { xc :: Double, yc :: Double, radius :: Double, startAngle :: Double, endAngle :: Double }
  | Rectangle { rx :: Double, ry :: Double, width :: Double, height :: Double }
  | Path [PathOp]
  deriving (Show, Eq)

data PathOp
  = CurveTo { x1 :: Double, y1 :: Double, x2 :: Double, y2 :: Double, x3 :: Double, y3 :: Double }
  | RelCurveTo { dx1 :: Double, dy1 :: Double, dx2 :: Double, dy2 :: Double, dx3 :: Double, dy3 :: Double }
  | LineTo { x :: Double, y :: Double }
  | RelLineTo { dx :: Double, dy :: Double }
  | MoveTo { x :: Double, y :: Double }
  | RelMoveTo { dx :: Double, dy :: Double }
  deriving (Show, Eq)

renderPrim :: Primitive -> Cairo.Render ()
renderPrim (Arc xc yc radius startAngle endAngle)    = Cairo.arc xc yc radius startAngle endAngle
renderPrim (ArcNeg xc yc radius startAngle endAngle) = Cairo.arcNegative xc yc radius startAngle endAngle
renderPrim (Rectangle x y w h)                       = Cairo.rectangle x y w h
renderPrim (Path operations)                         = renderPath operations

renderPrimitives :: [Primitive] -> Cairo.Render ()
renderPrimitives = mapM_ renderPrim

renderPathOp :: PathOp -> Cairo.Render ()
renderPathOp (CurveTo x1 y1 x2 y2 x3 y3)          = Cairo.curveTo x1 y1 x2 y2 x3 y3
renderPathOp (RelCurveTo dx1 dy1 dx2 dy2 dx3 dy3) = Cairo.relCurveTo dx1 dy1 dx2 dy2 dx3 dy3
renderPathOp (LineTo x y)                         = Cairo.lineTo x y
renderPathOp (RelLineTo dx dy)                    = Cairo.relLineTo dx dy
renderPathOp (MoveTo x y)                         = Cairo.moveTo x y
renderPathOp (RelMoveTo dx dy)                    = Cairo.relMoveTo dx dy

renderPath :: [PathOp] -> Cairo.Render ()
renderPath = mapM_ renderPathOp

applyLineStyle :: LineStyle -> Cairo.Render ()
applyLineStyle style = do
  Cairo.setSourceRGB r g b
  Cairo.setLineWidth $ lineWidth style
  Cairo.setLineCap $ convertLineCap $ cap style
  Cairo.setLineJoin $ convertLineJoin $ join style
  Cairo.setDash (dash style) (dashOffset style)
  where
    (r, g, b) = color style

convertLineCap :: LineCap -> Cairo.LineCap
convertLineCap Flat = Cairo.LineCapButt
convertLineCap Round = Cairo.LineCapRound
convertLineCap Padded = Cairo.LineCapSquare

{- TODOOOOOOO FIX -}
convertLineJoin :: LineJoin -> Cairo.LineJoin
convertLineJoin _ = Cairo.LineJoinRound