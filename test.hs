import Graphics.Declarative.Shape
import Graphics.Declarative.Form
import GtkUtils

import Colors

import Automaton

import Utils

main = runGTK (mapBehavior render <<< foldEvents (const . not) False)
  where render x = circle 100 |> filled (if x then blue else red)
