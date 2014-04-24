module Main where

import Demos.DemoUtilities
import Graphics.Declarative.Form
import Graphics.Declarative.Combinators
import Graphics.Declarative.Util.SyntaxText
import Text.Highlighting.Kate.Styles

main :: IO ()
main = do
  source <- readFile "TestSource.hs"
  openGTK $ sourceCode source

sourceCode :: String -> Form
sourceCode source = groupBy toBottom $ 
  map makeSource [ pygments, kate, espresso, tango, haddock, monochrome, zenburn ]
  where
    makeSource style = padded 10 $ highlightedSource textStyle style "haskell" source
    textStyle = defaultTextStyle {
      fontFamily = "Monospace",
      fontSize = 8
    }