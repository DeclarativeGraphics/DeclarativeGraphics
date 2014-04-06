module Main where

import Graphics.UI.Gtk hiding (eventSent)
import Graphics.UI.Gtk.Gdk.Events (Event, eventSent)
import Graphics.Rendering.Cairo hiding (rectangle)
import ReactiveDraw
import DrawTypes (defaultLineStyle)

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
  draw $ outlined defaultLineStyle $ circle 20

  translate 100 100
  pContext <- liftIO $ cairoCreateContext Nothing
  pLayout <- liftIO $ layoutText pContext "This is a test."
  updateLayout pLayout
  showLayout pLayout
  return ()