module Main where

import Graphics.UI.Gtk hiding (eventSent)
import Graphics.UI.Gtk.Gdk.Events (Event, eventSent)
import Graphics.Rendering.Cairo hiding (rectangle)
import ReactiveDraw
import DrawTypes hiding (Color)

main :: IO ()
main = do
  initGUI
  window <- windowNew
  set window windowProperties

  canvas <- drawingAreaNew
  containerAdd window canvas

  widgetModifyBg canvas StateNormal white
  widgetShowAll window

  onExpose canvas (handleExpose canvas)
  onDestroy window mainQuit
  mainGUI 

handleExpose :: WidgetClass w => w -> Event -> IO Bool
handleExpose canvas event = do
  drawin <- widgetGetDrawWindow canvas
  renderWithDrawable drawin myDraw
  return (eventSent event)

white :: Color
white = Color 65535 65535 65535

windowProperties :: [AttrOp Window]
windowProperties = [
  windowTitle          := "Hello Cairo",
  windowDefaultWidth   := 800, 
  windowDefaultHeight  := 600,
  containerBorderWidth := 0 ]

myDraw :: Render ()
myDraw = do
  draw $ positionArbitrary [
    ((0, 0), {-debugEnvelope $-} filled (0.5, 0.5, 0.5) $ circle 20),
    ((40, 0), {-debugEnvelope $-} outlined defaultLineStyle { color = (0, 1, 0) } $ circle 25) ]

  let mytext = text "This is a test String..."

  draw $ positionArbitrary [((0, 50), mytext)]
  --draw $ positionArbitrary [((0, 50), showEnvelopeForm mytext)]

  return ()