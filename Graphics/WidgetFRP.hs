module Graphics.WidgetFRP where

import Graphics.FRP
import Graphics.FRPUtils

data WidgetEvent = MouseMove (Point Double)
                 | MousePress Mouse
                 | MouseRelease Mouse
                 | KeyPress Key
                 | KeyRelease Key
                 | Resize (Point Int)
                 deriving (Eq,Show)

data Key   = Letter Char | Return | Space deriving (Eq, Show)
data Mouse = MBLeft | MBMiddle | MBRight | MWUp | MWDown deriving (Eq, Show)

type WidgetFRP state = FRP (Event WidgetEvent) (Behavior state)

type Point a = (a,a)
type RGB a = (a,a,a)



--------- FRP ---------

resizeEvent :: FRP (Event WidgetEvent) (Event (Int,Int))
resizeEvent = eventExtract getResizeSize
  where
    getResizeSize (Resize size) = Just size
    getResizeSize _ = Nothing

size :: FRP (Event (Int,Int)) (Behavior (Int,Int))
size = hold (0,0)



---- MOUSE ----

mouseMove :: FRP (Event WidgetEvent) (Event (Point Double))
mouseMove = eventExtract getMouseMovePosition
  where
    getMouseMovePosition (MouseMove pos) = Just pos
    getMouseMovePosition _ = Nothing

mousePress :: FRP (Event WidgetEvent) (Event Mouse)
mousePress = eventExtract getMousePressButton
  where
    getMousePressButton (MousePress btn) = Just btn
    getMousePressButton _ = Nothing

mouseRelease :: FRP (Event WidgetEvent) (Event Mouse)
mouseRelease = eventExtract getMouseReleaseButton
  where
    getMouseReleaseButton (MouseRelease btn) = Just btn
    getMouseReleaseButton _ = Nothing

mousePos :: FRP (Event WidgetEvent) (Behavior (Point Double))
mousePos = hold (0,0) <<< mouseMove



---- KEY ----

keyPress :: FRP (Event WidgetEvent) (Event Key)
keyPress = eventExtract getKeyPressKey
  where
    getKeyPressKey (KeyPress key) = Just key
    getKeyPressKey _ = Nothing

keyRelease :: FRP (Event WidgetEvent) (Event Key)
keyRelease = eventExtract getKeyReleaseKey
  where
    getKeyReleaseKey (KeyRelease key) = Just key
    getKeyReleaseKey _ = Nothing



