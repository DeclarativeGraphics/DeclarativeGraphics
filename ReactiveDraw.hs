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
  envWidth :: Double,
  envHeight :: Double
} deriving (Show, Eq)

data Form = Form {
  fEnvelope :: Envelope,
  fDraw :: Cairo.Render ()
}

circle :: Double -> Shape
circle radius = Shape {
  sEnvelope = Envelope size size,
  sPrims = [Prim.Arc radius radius radius 0 (2 * pi)]
} where size = radius + radius

rectangle :: Double -> Double -> Shape
rectangle width height = Shape {
  sEnvelope = Envelope width height,
  sPrims = [Prim.Rectangle 0.5 0.5 (width-1) (height-1)]
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

{- Not really necessary? Exposes Cairo type
filledWithRule :: Cairo.FillRule -> Color -> Shape -> Form
filledWithRule rule (r, g, b) shape = Form {
  fEnvelope = sEnvelope shape,
  fDraw = do
    Cairo.save
    Cairo.setSourceRGB r g b
    Cairo.setFillRule rule
    Prim.renderPrimitives $ sPrims shape
    Cairo.fill
    Cairo.restore
}
-}

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
  fEnvelope = Envelope width height,
  fDraw = do
    Cairo.save
    showLayout pLayout
    Cairo.restore
} where
  pLayout :: PangoLayout
  pLayout = unsafePerformIO $ layoutText standardContext content
  (_, PangoRectangle _ _ width height) = unsafePerformIO $ layoutGetExtents pLayout

empty :: Shape
empty = blank 0 0

blank :: Double -> Double -> Shape
blank w h = onlyEnvelope $ Envelope w h

onlyEnvelope :: Envelope -> Shape
onlyEnvelope env = Shape env []

positioned :: [(Position, Form)] -> Form
positioned positionList = Form {
  fEnvelope = Envelope 0 0,
  fDraw = do
    mapM_ (\((x, y), form) -> do
      Cairo.save
      Cairo.translate x y
      fDraw form
      Cairo.restore) positionList
}

overlayed :: [Form] -> Form
overlayed rs = positioned $ map (\r -> ((0, 0), r)) rs

showEnvelopeShape :: Shape -> Shape
showEnvelopeShape shape = rectangle w h
  where (Envelope w h) = sEnvelope shape

showEnvelopeForm :: Shape -> Form
showEnvelopeForm = (outlined defaultLineStyle { color = (1, 0, 0) }) . showEnvelopeShape

-- Used for text drawing:
standardContext :: PangoContext
standardContext = unsafePerformIO $ cairoCreateContext Nothing