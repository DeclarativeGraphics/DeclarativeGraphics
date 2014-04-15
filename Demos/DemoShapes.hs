module Main where

import Demos.DemoUtilities
import Graphics.Declarative.Form
import Graphics.Declarative.Shape
import Graphics.Declarative.Combinators

main :: IO ()
main = openGTK picture
--main = createTitleImage picture

picture :: Form
picture = groupBy downAttach [
  centered $ text defaultTextStyle "groupBy rightAttach",
  debugEnvelope $ padded 4 $ groupBy rightAttach [ orangeCircle, greenRectangle ],
  centered $ text defaultTextStyle "groupBy leftAttach",
  debugEnvelope $ padded 4 $ groupBy leftAttach [ orangeCircle, greenRectangle ] ]
  where
    thickOrangeLine = defaultLineStyle { color = (1, 0.5, 0), lineWidth = 2 }
    orangeCircle = outlined thickOrangeLine $ circle 40
    greenRectangle = outlined (solid (0, 1, 0)) $ rectangle 80 80