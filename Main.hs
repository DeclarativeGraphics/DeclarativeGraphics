module Main where

import Graphics.UI.Gtk hiding (eventSent)
import Graphics.UI.Gtk.Gdk.Events (Event, eventSent)
import Graphics.Rendering.Cairo hiding (rectangle)
import Graphics.Declarative.Form hiding (Color)
import Graphics.Declarative.Shape
import Graphics.Declarative.Combinators

main :: IO ()
main = openGTK picture0
--main = createTitleImage picture0

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
  fDraw $ moved (w / 2, h / 2) $ {-debugEnvelope $-} form
  --liftIO $ print $ fEnvelope form

picture0 :: Form
picture0 = centered $
  groupBy downAttach [
    centered $ text "groupBy rightAttach",
    debugEnvelope $ padded 4 $ groupBy rightAttach [ formA, formB ],
    centered $ text "groupBy leftAttach",
    debugEnvelope $ padded 4 $ groupBy leftAttach [ formA, formB ] ]
  where
    formA = outlined defaultLineStyle { color = (1, 0.5, 0), lineWidth = 2 } $ circle 40
    formB = outlined (solid (0, 1, 0)) $ rectangle 80 80

picture1 :: Form
picture1 = outlined defaultLineStyle { color = (1, 0.5, 0), lineWidth = 2 } $ circle 40

picture2 :: Form
picture2 = groupBy downAttach [
  outlined defaultLineStyle { color = (1, 0.5, 0), lineWidth = 2 } $ circle 40,
  outlined (solid (0, 1, 0)) $ rectangle 50 50 ]

picture3 :: Form
picture3 = centered $
  groupBy downAttach [
    outlined defaultLineStyle { color = (1, 0.5, 0), lineWidth = 2 } $ circle 40,
    outlined (solid (0, 1, 0)) $ rectangle 50 50 ]