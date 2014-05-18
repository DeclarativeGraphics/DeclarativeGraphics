module KeyboardInput where

import Debug.Trace

import qualified Graphics.UI.Gtk as G

data KeyboardInput = Letter Char
                   | Special SpecialKey
                   deriving (Show)

data SpecialKey = Escape | Backspace | Shift
                | ArrLeft | ArrUp | ArrRight | ArrDown
                deriving (Show)


keyboardInputFromGdk :: G.KeyVal -> Maybe KeyboardInput
keyboardInputFromGdk k = maybe parseSpecialKey parseLetter (G.keyToChar k)
  where 
    parseLetter c = Just $ Letter c
    parseSpecialKey = case G.keyName k of
      "Escape"    -> Just $ Special Escape
      "BackSpace" -> Just $ Special Backspace
      "Shift_L"   -> Just $ Special Shift
      unknown     -> (trace ("Don't know how to interpret GDK key " ++ show unknown) Nothing)
