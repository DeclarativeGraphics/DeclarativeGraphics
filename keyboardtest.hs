import Graphics.Declarative.Shape
import Graphics.Declarative.Form
import GtkUtils

import Colors

import Automaton

import Utils

import KeyboardInput

main = runGTK (mapBehavior render <<< holdLast (KeyPress (Letter 'x')))
  where render' x = circle 100 |> filled (if x then blue else red)
        render keyboardInput = text defaultTextStyle (show keyboardInput)
