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
  {-
  let midx = 400
      midy = 300
  translate midx midy
  rotate $ (3 / 4) * pi
  fDraw $ positioned [
    ((0, 0), outlinedCol (1, 0, 0) $ circle 20),
    ((40, 0), text "This is another test...") ]
  translate (-midx) (-midy)
  -}
  fDraw $ outlined defaultLineStyle { color = (1, 0.5, 0), lineWidth = 5 } $ circle 40

  return ()