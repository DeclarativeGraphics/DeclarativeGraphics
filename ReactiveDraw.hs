module ReactiveDraw where

import qualified Graphics.Rendering.Cairo as Cairo
import qualified Primitive as Prim
import Graphics.UI.Gtk hiding (Color)
import System.IO.Unsafe (unsafePerformIO)
import DrawTypes

data Shape = Shape {
  envelope :: Envelope,
  prims :: [Prim.Primitive]
} deriving (Show, Eq)

data Form = Form {
  formification :: Prim.Formify,
  shape :: Shape
} deriving (Show, Eq)

data Envelope = Envelope {
  envWidth :: Double,
  envHeight :: Double
} deriving (Show, Eq)


circle :: Double -> Shape
circle radius = Shape {
  envelope = Envelope size size,
  prims = [Prim.Arc radius radius radius 0 (2 * pi)]
} where size = radius + radius

rectangle :: Double -> Double -> Shape
rectangle width height = Shape {
  envelope = Envelope width height,
  prims = [Prim.Rectangle 0 0 width height]
}

filled :: Color -> Shape -> Form
filled = filledWithRule Cairo.FillRuleWinding

filledWithRule :: Cairo.FillRule -> Color -> Shape -> Form
filledWithRule rule col s = Form {
  formification = Prim.Fill col rule,
  shape = s
}

outlined :: LineStyle -> Shape -> Form
outlined style s = Form {
  formification = Prim.Stroke style,
  shape = s
}


draw :: Form -> Cairo.Render ()
draw form = do
  Prim.renderPrimitives $ prims $ shape form
  Prim.applyFormify $ formification form

{-
empty :: Graphic
empty = blank 0 0

blank :: Double -> Double -> Graphic
blank w h = Graphic {
  envelope = Envelope w h,
  draw = return ()
}

circle :: PathStyle -> Double -> Graphic
circle style radius = Graphic {
  envelope = Envelope size size,
  draw = do
    circlePath radius
    evaluateStyle style
} where size = radius + radius

rectangle :: PathStyle -> Double -> Double -> Graphic
rectangle style width height = Graphic {
  envelope = Envelope width height,
  draw = do
    Cairo.save
    Cairo.translate 0.5 0.5
    rectanglePath (width - 1) (height - 1)
    evaluateStyle style
    Cairo.restore
}

text :: String -> Graphic
text content = Graphic {
  envelope = Envelope width height,
  draw = do
    Cairo.save
    showLayout pLayout
    Cairo.restore
} where
  pLayout :: PangoLayout
  pLayout = unsafePerformIO $ layoutText standardContext content
  (_, PangoRectangle _ _ width height) = unsafePerformIO $ layoutGetExtents pLayout

besides :: Axis -> Double -> [Graphic] -> Graphic
besides _ _ [] = empty
besides axis gap graphics = Graphic {
  envelope = 
    case axis of
      Horiz -> Envelope ((sum $ map (envWidth . envelope) graphics) + totalGaps) (maximum $ map (envHeight . envelope) graphics)
      Vert  -> Envelope (maximum $ map (envWidth . envelope) graphics) ((sum $ map (envHeight . envelope) graphics) + totalGaps),
  draw = do
    Cairo.save
    mapM (\graphic -> do
      let Envelope gWidth gHeight = envelope graphic
      draw graphic
      case axis of
        Horiz -> Cairo.translate (gWidth + gap) 0
        Vert  -> Cairo.translate 0 (gHeight + gap))
      graphics
    Cairo.restore
} where totalGaps = (fromIntegral $ length graphics) * gap

circlePath :: Double -> Cairo.Render ()
circlePath radius = do
  Cairo.arc
    radius   -- center x
    radius   -- center y
    radius   -- the actual radius
    0        -- start angle in radians
    (2 * pi) -- end angle in radians

rectanglePath :: Double -> Double -> Cairo.Render ()
rectanglePath width height = Cairo.rectangle 0 0 width height

evaluateStyle :: PathStyle -> Cairo.Render ()
evaluateStyle Stroke = Cairo.stroke
evaluateStyle Filled = Cairo.fill

standardContext :: PangoContext
standardContext = unsafePerformIO $ cairoCreateContext Nothing-}