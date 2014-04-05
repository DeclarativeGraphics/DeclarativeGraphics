module ReactiveDraw where

import Graphics.Rendering.Cairo
import FRP

data Shape frp draw = Shape {
  shapeReaction :: frp
  shapeRenderer :: draw
}