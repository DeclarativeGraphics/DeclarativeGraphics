module Demos.DemoUtilities where

import Graphics.UI.Gtk hiding (eventSent)
import Graphics.UI.Gtk.Gdk.Events (Event, eventSent)
import Graphics.Rendering.Cairo hiding (rectangle)
import Graphics.Declarative.Form hiding (Color)

createTitleImage :: Form -> IO ()
createTitleImage form = makeSvg form "titleImage.svg" 400 300

makeSvg :: Form -> FilePath -> Double -> Double -> IO ()
makeSvg form path w h = withSVGSurface path (realToFrac w) (realToFrac h) (\surface -> renderWith surface (drawForm form w h))

openGTK :: Form -> IO ()
openGTK form = do
  initGUI
  window <- windowNew
  set window windowProperties

  canvas <- drawingAreaNew
  containerAdd window canvas

  widgetModifyBg canvas StateNormal white
  widgetShowAll window

  onExpose canvas (handleExpose form canvas)
  onDestroy window mainQuit
  mainGUI 

handleExpose :: WidgetClass w => Form -> w -> Event -> IO Bool
handleExpose form canvas event = do
  (w, h) <- widgetGetSize canvas
  drawin <- widgetGetDrawWindow canvas
  renderWithDrawable drawin (drawForm form (fromIntegral w) (fromIntegral h))
  return (eventSent event)

white :: Color
white = Color 65535 65535 65535

windowProperties :: [AttrOp Window]
windowProperties = [
  windowTitle          := "Hello Cairo",
  windowDefaultWidth   := 800, 
  windowDefaultHeight  := 600,
  containerBorderWidth := 0 ]

drawForm :: Form -> Double -> Double -> Render ()
drawForm form w h = do
  fDraw $ moved (w / 2, h / 2) $ centered $ form
