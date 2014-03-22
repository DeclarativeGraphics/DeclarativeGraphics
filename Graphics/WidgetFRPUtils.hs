module Graphics.WidgetFRPUtils where

import Control.Monad

import Graphics.FRP
import Graphics.FRPUtils
import Graphics.WidgetFRP

relMove :: FRP (Event (Point Double)) (Event (Point Double))
relMove = (lastRelMove `sampleOn` input) >>> eventExtract id
  where
    lastRelMove = maybeLastEvents 2 >>> mapBehavior calcMove
    calcMove [posnew, posold] = liftM2 posSub posnew posold
    posSub (xn,yn) (xo,yo)  = (xn - xo, yn - yo)

mousePressed :: Mouse -> FRP (Event WidgetEvent) (Behavior Bool)
mousePressed button = foldp isMousePressed False
  where
    isMousePressed event isPressed = case event of
      MousePress   b | b == button -> True
      MouseRelease b | b == button -> False
      _                            -> isPressed

drag :: Mouse -> FRP (Event WidgetEvent) (Event (Point Double))
drag button = (mouseMove >>> relMove) `filterWhen` mousePressed button


movePosition :: Point Double -> FRP a (Event (Point Double)) -> FRP a (Behavior (Point Double))
movePosition init relMoves = foldp movePos init <<< relMoves
  where movePos (dx, dy) (x, y) = (dx + x, dy + y)
