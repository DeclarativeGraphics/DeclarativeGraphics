module Graphics.Declarative.Internal.Primitive where

import qualified Graphics.Rendering.Cairo as Cairo

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
renderPrim (Arc arcXc arcYc arcRadius arcStartAngle arcEndAngle)    = Cairo.arc arcXc arcYc arcRadius arcStartAngle arcEndAngle
renderPrim (ArcNeg arcXc arcYc arcRadius arcStartAngle arcEndAngle) = Cairo.arcNegative arcXc arcYc arcRadius arcStartAngle arcEndAngle
renderPrim (Rectangle posx posy w h)                                = Cairo.rectangle posx posy w h
renderPrim (Path operations)                                        = renderPath operations

renderPrimitives :: [Primitive] -> Cairo.Render ()
renderPrimitives = mapM_ renderPrim

renderPathOp :: PathOp -> Cairo.Render ()
renderPathOp (CurveTo cx1 cy1 cx2 cy2 cx3 cy3)          = Cairo.curveTo cx1 cy1 cx2 cy2 cx3 cy3
renderPathOp (RelCurveTo cdx1 cdy1 cdx2 cdy2 cdx3 cdy3) = Cairo.relCurveTo cdx1 cdy1 cdx2 cdy2 cdx3 cdy3
renderPathOp (LineTo posx posy)                         = Cairo.lineTo posx posy
renderPathOp (RelLineTo cdx cdy)                        = Cairo.relLineTo cdx cdy
renderPathOp (MoveTo mx posy)                           = Cairo.moveTo mx posy
renderPathOp (RelMoveTo mdx mdy)                        = Cairo.relMoveTo mdx mdy

renderPath :: [PathOp] -> Cairo.Render ()
renderPath = mapM_ renderPathOp
