module Primitive where

import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Rendering.Cairo.Types as Cairo
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

data Formify
  = Clip
  | Fill Color Cairo.FillRule
  | Stroke LineStyle
  | Mask Cairo.Pattern
  | MaskSurface Cairo.Surface Double Double

instance Show Formify where
  show Clip = "Clip"
  show (Fill col _) = "Fill " ++ show col ++ " *rule*"
  show (Stroke style) = "Stroke " ++ show style
  show (Mask _) = "Mask *pattern*"
  show (MaskSurface _ xorig yorig) = "MaskSurface *surface*" ++ show xorig ++ " " ++ show yorig

instance Eq Formify where
  Clip == Clip = True
  (Fill col1 rule1) == (Fill col2 rule2) = col1 == col2 && rule1 == rule2
  (Stroke style1) == (Stroke style2) = style1 == style2
  (Mask (Cairo.Pattern ptr1)) == (Mask (Cairo.Pattern ptr2)) = ptr1 == ptr2
  (MaskSurface (Cairo.Surface ptr1) xo1 yo1) == (MaskSurface (Cairo.Surface ptr2) xo2 yo2) = ptr1 == ptr2 && xo1 == xo2 && yo1 == yo2
  _ == _ = False

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

applyFormify :: Formify -> Cairo.Render ()
applyFormify Clip = Cairo.clip
applyFormify (Stroke style) = applyLineStyle style >> Cairo.stroke
applyFormify (Fill (r, g, b) rule) = do
  Cairo.setFillRule rule
  Cairo.setSourceRGB r g b
  Cairo.fill

applyLineStyle :: LineStyle -> Cairo.Render ()
applyLineStyle style = do
  Cairo.setSourceRGB r g b
  Cairo.setLineWidth $ lineWidth style
  Cairo.setLineCap $ convertLineCap $ cap style
  Cairo.setLineJoin $ convertLineJoin $ join style
  {- TODOOO Dash -}
  where
    (r, g, b) = color style

convertLineCap :: LineCap -> Cairo.LineCap
convertLineCap Flat = Cairo.LineCapButt
convertLineCap Round = Cairo.LineCapRound
convertLineCap Padded = Cairo.LineCapSquare

{- TODOOOOOOO FIX -}
convertLineJoin :: LineJoin -> Cairo.LineJoin
convertLineJoin _ = Cairo.LineJoinRound