module Main where

import Graphics.UI.Gtk hiding (eventSent)
import Graphics.UI.Gtk.Gdk.Events (Event, eventSent)
import Graphics.Rendering.Cairo hiding (rectangle)
import ReactiveDraw

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
  fDraw $ positioned [
    ((0, 0), outlinedCol (1, 0, 0) $ circle 20),
    ((40, 0), text "This is another test...")]

  return ()