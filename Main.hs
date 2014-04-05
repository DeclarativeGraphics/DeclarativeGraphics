module Main where

import Graphics.UI.Gtk hiding (eventSent)
import Graphics.UI.Gtk.Gdk.Events (Event, eventSent)
import Graphics.Rendering.Cairo
import ReactiveDraw

main :: IO ()
main = do
  initGUI
  window <- windowNew
  set window windowProperties

  frame <- frameNew
  containerAdd window frame

  canvas <- drawingAreaNew
  containerAdd frame canvas

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
  setSourceRGB 0 0 0
  setLineWidth 1
  translate 10 10
  draw $ rectangleOutlined 50 40
  translate 50 0
  draw $ circleOutlined 20