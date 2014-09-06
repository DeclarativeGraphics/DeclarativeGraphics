module KeyboardInput where

import System.Glib.UTFString
import Debug.Trace

import qualified Graphics.UI.Gtk as G

data KeyboardInput = Letter Char
                   | Special SpecialKey
                   deriving (Show, Eq)

data SpecialKey = Return | Escape | Backspace | Shift
                | ArrLeft | ArrUp | ArrRight | ArrDown
                deriving (Show, Eq)


keyboardInputFromGdk :: G.KeyVal -> Maybe KeyboardInput
keyboardInputFromGdk k = maybe parseSpecialKey parseLetter (G.keyToChar k)
  where 
    parseLetter c = Just $ Letter c
    parseSpecialKey = case glibToString $ G.keyName k of
      "Return"    -> Just $ Special Return
      "Escape"    -> Just $ Special Escape
      "BackSpace" -> Just $ Special Backspace
      "Shift_L"   -> Just $ Special Shift
      "Left"      -> Just $ Special ArrLeft
      "Right"     -> Just $ Special ArrRight
      "Down"      -> Just $ Special ArrDown
      "Up"        -> Just $ Special ArrUp
      unknown     -> (trace ("Don't know how to interpret GDK key " ++ show unknown) Nothing)
