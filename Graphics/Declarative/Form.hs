module Graphics.Declarative.Form where

import System.IO.Unsafe (unsafePerformIO)
import Graphics.Rendering.Pango hiding (Color)
import qualified Graphics.Rendering.Cairo as Cairo
import qualified Graphics.Declarative.Internal.Primitive as Prim
import Graphics.Declarative.Envelope
import Graphics.Declarative.Shape

type Position = (Double, Double)
type Color = (Double, Double, Double)

data Form = Form {
  fEnvelope :: Envelope,
  fDraw :: Cairo.Render ()
}

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
data LineJoin = Clipped | Smooth | Sharp deriving (Show, Eq)


filled :: Color -> Shape -> Form
filled (r, g, b) shape = Form {
  fEnvelope = sEnvelope shape,
  fDraw = do
    Cairo.save
    Cairo.setSourceRGB r g b
    Prim.renderPrimitives $ sPrims shape
    Cairo.fill
    Cairo.restore
}

outlined :: LineStyle -> Shape -> Form
outlined style shape = Form {
  fEnvelope = sEnvelope shape,
  fDraw = do
    Cairo.save
    applyLineStyle style
    Prim.renderPrimitives $ sPrims shape
    Cairo.stroke
    Cairo.restore
}

outlinedCol :: Color -> Shape -> Form
outlinedCol col = outlined defaultLineStyle { color = col }

text :: String -> Form
text content = Form {
  fEnvelope = Envelope 0 0 width height,
  fDraw = do
    Cairo.save
    showLayout pLayout
    Cairo.restore
} where
  pLayout :: PangoLayout
  pLayout = unsafePerformIO $ layoutText standardContext content
  (_, PangoRectangle _ _ width height) = unsafePerformIO $ layoutGetExtents pLayout

-- Used for text drawing:
standardContext :: PangoContext
standardContext = unsafePerformIO $ cairoCreateContext Nothing

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

convertLineJoin :: LineJoin -> Cairo.LineJoin
convertLineJoin Clipped = Cairo.LineJoinBevel
convertLineJoin Smooth = Cairo.LineJoinRound
convertLineJoin Sharp = Cairo.LineJoinMiter