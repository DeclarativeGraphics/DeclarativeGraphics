module Graphics.Declarative.Util.FromSvg where

import System.IO
import Graphics.Rendering.Cairo.SVG
import Graphics.Rendering.Cairo (liftIO)

import Graphics.Declarative.Form
import Graphics.Declarative.Envelope

formFromSvg :: SVG -> Form
formFromSvg svg = Form {
  fEnvelope = let (w, h) = svgGetSize svg in Envelope 0 0 (fromIntegral w) (fromIntegral h),
  fDraw = do
    success <- svgRender svg
    if success
      then return ()
      else liftIO $ putStrLn "Warning: Couldn't render SVG. (Graphics.Declarative.Util.FromSVG)"
}

formSvgFromString :: String -> IO Form
formSvgFromString str = formFromSvg `fmap` svgNewFromString str

formSvgFromHandle :: Handle -> IO Form
formSvgFromHandle handle = formFromSvg `fmap` svgNewFromHandle handle

formSvgFromFile :: FilePath -> IO Form
formSvgFromFile file = formFromSvg `fmap` svgNewFromFile file
