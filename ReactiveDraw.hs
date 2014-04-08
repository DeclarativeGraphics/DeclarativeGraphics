module ReactiveDraw where

import qualified Graphics.Rendering.Cairo as Cairo
import qualified Primitive as Prim
import Graphics.UI.Gtk hiding (Color)
import System.IO.Unsafe (unsafePerformIO)
import DrawTypes

class Renderable a where
  envelope :: a -> Envelope
  draw :: a -> Cairo.Render ()

data Shape = Shape {
  sEnvelope :: Envelope,
  sPrims :: [Prim.Primitive]
} deriving (Show, Eq)

data Envelope = Envelope {
  envWidth :: Double,
  envHeight :: Double
} deriving (Show, Eq)

data Form = Form {
  fFormification :: Prim.Formify,
  fShape :: Shape
} deriving (Show, Eq)

data Text = Text {
  tEnvelope :: Envelope,
  tContent :: String
}

type Position = (Double, Double)

data Collage renderable = Collage {
  cEnvelope :: Envelope,
  cForms :: [(Position, renderable)]
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
filled = filledWithRule Cairo.FillRuleWinding

filledWithRule :: Cairo.FillRule -> Color -> Shape -> Form
filledWithRule rule col s = Form {
  fFormification = Prim.Fill col rule,
  fShape = s
}

outlined :: LineStyle -> Shape -> Form
outlined style s = Form {
  fFormification = Prim.Stroke style,
  fShape = s
}

{- TODO: Think... Create PangoLayout twice, or save it inside data Text? -}
text :: String -> Text
text content = Text {
  tEnvelope = Envelope width height,
  tContent = content
} where
  pLayout :: PangoLayout
  pLayout = unsafePerformIO $ layoutText standardContext content
  (_, PangoRectangle _ _ width height) = unsafePerformIO $ layoutGetExtents pLayout

instance Renderable Form where
  envelope = sEnvelope . fShape
  draw (Form formification shape) = do
    Prim.renderPrimitives $ sPrims $ shape
    Prim.applyFormify $ formification

instance Renderable f => Renderable (Collage f) where
  envelope = cEnvelope
  draw (Collage _ renderables) = do
    mapM_ (\((x, y), renderable) -> do
      Cairo.save
      Cairo.translate x y
      draw renderable
      Cairo.restore) renderables

instance Renderable Text where
  envelope = tEnvelope
  draw (Text _ content) = do
    pLayout <- Cairo.liftIO $ layoutText standardContext content
    Cairo.save
    showLayout pLayout
    Cairo.restore

empty :: Shape
empty = blank 0 0

blank :: Double -> Double -> Shape
blank w h = onlyEnvelope $ Envelope w h

onlyEnvelope :: Envelope -> Shape
onlyEnvelope env = Shape env []

{- TODO: Not that easy again.. What if somebody wants to create a Collage with Text and Form? -}
positionArbitrary :: Renderable r => [(Position, r)] -> Collage r
positionArbitrary positionList = Collage {
  cEnvelope = Envelope 0 0,
  cForms = positionList
}

collageSingleton :: Renderable r => r -> Collage r
collageSingleton renderable = Collage {
  cEnvelope = envelope renderable,
  cForms = [((0, 0), renderable)]
}

overlay :: Renderable r => [r] -> Collage r
overlay rs = positionArbitrary $ map (\r -> ((0, 0), r)) rs

showEnvelopeShape :: Renderable r => r -> Shape
showEnvelopeShape r = rectangle w h
  where (Envelope w h) = envelope r

showEnvelopeForm :: Renderable r => r -> Form
showEnvelopeForm = (outlined defaultLineStyle { color = (1, 0, 0) }) . showEnvelopeShape

debugEnvelope :: Form -> Collage Form
debugEnvelope form = overlay [form, showEnvelopeForm form]

-- Used for text drawing:
standardContext :: PangoContext
standardContext = unsafePerformIO $ cairoCreateContext Nothing

{-

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
-}