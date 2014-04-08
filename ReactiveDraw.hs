module ReactiveDraw where

import qualified Graphics.Rendering.Cairo as Cairo
import qualified Primitive as Prim
import Graphics.UI.Gtk hiding (Color)
import System.IO.Unsafe (unsafePerformIO)
import DrawTypes

type Position = (Double, Double)

data Shape = Shape {
  sEnvelope :: Envelope,
  sPrims :: [Prim.Primitive]
} deriving (Show, Eq)

data Envelope = Envelope {
  envToLeft :: Double,
  envToTop :: Double,
  envToRight :: Double,
  envToBottom :: Double
} deriving (Show, Eq)

data Form = Form {
  fEnvelope :: Envelope,
  fDraw :: Cairo.Render ()
}

envelopeCenteredRect :: Double -> Double -> Envelope
envelopeCenteredRect width height = Envelope (-width / 2) (-height / 2) (width / 2) (height / 2)

envelopeCenteredCircle :: Double -> Envelope
envelopeCenteredCircle radius = Envelope (-radius) (-radius) radius radius

circle :: Double -> Shape
circle radius = Shape {
  sEnvelope = envelopeCenteredCircle radius,
  sPrims = [Prim.Arc 0 0 radius 0 (2 * pi)]
}

rectangle :: Double -> Double -> Shape
rectangle width height = Shape {
  sEnvelope = envelopeCenteredRect width height,
  sPrims = [Prim.Rectangle (-width/2) (-height/2) width height]
}

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
    Prim.applyLineStyle style
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

empty :: Shape
empty = blankCentered 0 0

blankCentered :: Double -> Double -> Shape
blankCentered w h = onlyEnvelope $ Envelope (-w/2) (-h/2) (w/2) (h/2)

onlyEnvelope :: Envelope -> Shape
onlyEnvelope env = Shape env []

-- Used for text drawing:
standardContext :: PangoContext
standardContext = unsafePerformIO $ cairoCreateContext Nothing