module ReactiveDraw where

import qualified Graphics.Rendering.Cairo as Cairo
import FRP

data Graphic = Graphic {
  envelope :: Envelope,
  draw :: Cairo.Render ()
}

data Envelope = Envelope {
  envWidth :: Double,
  envHeight :: Double
}

circle :: Bool -> Double -> Graphic
circle filled radius = Graphic {
  envelope = Envelope size size,
  draw = do
    circlePath radius
    if filled then Cairo.fill else Cairo.stroke
} where size = radius * radius

circleOutlined :: Double -> Graphic
circleOutlined = circle False

circleFilled :: Double -> Graphic
circleFilled = circle True

rectangle :: Bool -> Double -> Double -> Graphic
rectangle filled width height = Graphic {
  envelope = Envelope width height,
  draw = do
    rectanglePath width height
    if filled then Cairo.fill else Cairo.stroke
}

rectangleOutlined :: Double -> Double -> Graphic
rectangleOutlined = rectangle False

rectangleFilled :: Double -> Double -> Graphic
rectangleFilled = rectangle True

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