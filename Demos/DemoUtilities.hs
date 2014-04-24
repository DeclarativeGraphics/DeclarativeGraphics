module Demos.DemoUtilities where

import Graphics.UI.Gtk hiding (eventSent)
import Graphics.UI.Gtk.Gdk.Events (Event, eventSent)
import Graphics.Rendering.Cairo hiding (rectangle)
import Graphics.Declarative.Form hiding (Color)
import Graphics.Declarative.Envelope
import Graphics.Declarative.Combinators
import Graphics.Declarative.Util.SyntaxText

makeSvg :: Form -> FilePath -> IO ()
makeSvg form path = withSVGSurface path (realToFrac w) (realToFrac h) (\surface -> renderWith surface (drawForm form w h))
  where
    (ws, hs) = envelopeSize $ fEnvelope form
    w = ws + 10
    h = hs + 10

openGTK :: Form -> IO ()
openGTK form = do
  let (w, h) = envelopeSize $ fEnvelope form
  initGUI
  window <- windowNew
  set window (windowProperties (w+10) (h+10))

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

windowProperties :: Double -> Double -> [AttrOp Window]
windowProperties width height = [
  windowTitle          := "Hello Cairo",
  windowDefaultWidth   := (ceiling width), 
  windowDefaultHeight  := (ceiling height),
  containerBorderWidth := 0 ]

drawForm :: Form -> Double -> Double -> Render ()
drawForm form w h = do
  fDraw $ moved (w / 2, h / 2) $ centered $ form

showSourceAndProduct :: String -> Form -> Form
showSourceAndProduct source result = groupBy toRight [ highlightedHaskell source, result ]