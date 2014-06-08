module Graphics.Declarative.Util.Svg where

import System.IO
import Graphics.Rendering.Cairo.SVG
import Graphics.Rendering.Cairo (liftIO)

import Graphics.Declarative.Form
import Graphics.Declarative.Envelope

-- Origin on top-left
svgForm :: SVG -> Form
svgForm svg = Form {
  fEnvelope = let (w, h) = svgGetSize svg in Envelope 0 0 (fromIntegral w) (fromIntegral h),
  fDraw = do
    success <- svgRender svg
    if success
      then return ()
      else liftIO $ putStrLn "Warning: Couldn't render SVG. (Graphics.Declarative.Util.SVG)"
}

-- Origin on top-left
svgFormString :: String -> IO Form
svgFormString str = svgForm `fmap` svgNewFromString str

-- Origin on top-left
svgFormHandle :: Handle -> IO Form
svgFormHandle handle = svgForm `fmap` svgNewFromHandle handle

-- Origin on top-left
svgFormFile :: FilePath -> IO Form
svgFormFile file = svgForm `fmap` svgNewFromFile file
