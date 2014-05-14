module Input where

import Char
import Keyboard

import MaybeUtils (..)

data Input = Letter Char
           | Special SpecialKey

data SpecialKey = Escape | Backspace | Shift
                | ArrLeft | ArrUp | ArrRight | ArrDown

interpretSpecialKey : Keyboard.KeyCode -> Maybe SpecialKey
interpretSpecialKey keycode = case keycode of
  8  -> Just Backspace
  16 -> Just Shift
  27 -> Just Escape
  --
  37 -> Just ArrLeft
  38 -> Just ArrUp
  39 -> Just ArrRight
  40 -> Just ArrDown
  --
  _  -> Nothing

input : Signal (Maybe Input)
input = let interpretKeyCode isShiftPressed code
              = interpretSpecialKey code
                     |> maybe (maybeDo Letter <| fixedFromCode isShiftPressed code)
                              (Just . Special)
        in lift2 interpretKeyCode Keyboard.shift Keyboard.lastPressed

fixedFromCode : Bool -> Keyboard.KeyCode -> Maybe Char
fixedFromCode isShiftPressed code =
  let transformChar = if isShiftPressed then Char.toUpper
                                        else Char.toLower
  in case (code, isShiftPressed) of
       (187, False) -> Just '='
       (187, True)  -> Just '+'

       _ -> if 65 <= code && code <= 90
            then Just <| transformChar <| Char.fromCode code
            else Nothing
