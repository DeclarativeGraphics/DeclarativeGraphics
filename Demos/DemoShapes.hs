module Main where

import Demos.DemoUtilities
import Graphics.Declarative.Form
import Graphics.Declarative.Shape
import Graphics.Declarative.Combinators

main :: IO ()
main = do
  source <- readFile "Demos/DemoShapes.hs"
  openGTK $ showSourceAndProduct source picture
-- main = createTitleImage picture

createTitleImage :: Form -> IO ()
createTitleImage form = makeSvg form "titleImage.svg"

picture :: Form
picture = groupBy toBottom [
  centered $ text defaultTextStyle "groupBy toRight",
  debugEnvelope $ padded 4 $ groupBy toRight [ orangeCircle, greenRectangle ],
  centered $ text defaultTextStyle "groupBy toLeft",
  debugEnvelope $ padded 4 $ groupBy toLeft [ orangeCircle, greenRectangle ] ]
  where
    thickOrangeLine = defaultLineStyle { color = (1, 0.5, 0), lineWidth = 2 }
    orangeCircle = outlined thickOrangeLine $ circle 40
    greenRectangle = outlined (solid (0, 1, 0)) $ rectangle 80 80
