module Graphics.CairoGraphics where

import Graphics.Point
import Graphics.Graphics

import Control.Monad

import Graphics.Rendering.Cairo

render :: Canvas -> Render ()
render (Canvas w h forms)
  = mapM_ (renderForm w h) forms
render (Text color size str)
  = setFontSize size >> setColor color >> moveTo 0 size >> showText str

renderForm :: Double -> Double -> Form -> Render ()
renderForm w h (Filled color shape)
  = sketch w h shape >> setColor color >> fill
renderForm w h (Outlined strokeoptions color shape)
  = sketch w h shape >> setColor color >> setStroke strokeoptions >> stroke
renderForm w h (AsForm canvas pos)
  = transformed (onCoords w h translate pos) $ render canvas

sketch :: Double -> Double -> Shape -> Render ()
sketch w h (ClosedPath (start : points))
  = onCoords w h moveTo start >> sketchPath w h points >> closePath
sketch w h (OpenPath (start : points))
  = onCoords w h moveTo start >> sketchPath w h points
sketch w h (Circle radius pos)
  = onCoords w h arc pos radius 0 (2*pi)

sketchPath w h = mapM_ (onCoords w h lineTo)

setStroke DefaultStrokeOptions = return ()

setColor (RGB r g b) = setSourceRGB r g b

transformed :: Render () -> Render a -> Render a
transformed transformation action = do
  save
  transformation
  result <- action
  restore
  return result
