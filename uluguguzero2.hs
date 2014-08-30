{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Data.Maybe

import FRPZero
import FRPZeroGtk
import GtkUtils

import Graphics.Declarative.Envelope
import Graphics.Declarative.Shape
import Graphics.Declarative.Form
import Graphics.Declarative.Combinators

import Control.Monad (liftM)

import Colors
import Utils
import TextInput
import KeyboardInput


renderShow :: (Show s) => s -> Form
renderShow = renderString . show

renderString :: String -> Form
renderString = text defaultTextStyle

besides ls = centered $ groupBy toRight ls
besides2 x y = centered $ groupBy toRight [x, y]

renderTextInput :: TextInput -> Form
renderTextInput (l,r) = centered <| groupBy toRight [centered leftText, cursor, centered rightText]
  where
    leftText  = text defaultTextStyle (reverse l)
    rightText = text defaultTextStyle r
    cursor = rectangle 1 20 |> filled black |> flip withEnvelope emptyEnvelope

      
modeTextInputWidgetTest
  = fuckingBigStuff >>^ accum (False, focus (map toTextInput ["foo", "bar", "spam", "eggs"]))
                    ^>> renderStuff
  where
    fuckingBigStuff event (False, textInputs) = progressOuter event (False, textInputs)
    fuckingBigStuff event (True, textInputs) = progressInner event (True, textInputs)

    progressInner (KeyPress key) (True, textInputs) = case key of
      Letter c         -> (True, onFocused (textInputInsert c) textInputs)
      Special Escape   -> (False, textInputs)
      Special ArrLeft  -> let focus' = textInputMoveLeft (getFocused textInputs)
                          in case focus' of
                            Nothing -> progressOuter (KeyPress key) (True, textInputs)
                            Just x  -> (True, setFocused x textInputs)

      Special ArrRight -> let focus' = textInputMoveRight (getFocused textInputs)
                          in case focus' of
                            Nothing -> progressOuter (KeyPress key) (True, textInputs)
                            Just x  -> (True, setFocused x textInputs)
      _                -> progressOuter (KeyPress key) (True, textInputs)
    progressInner event state = progressOuter event state

    progressOuter (KeyPress (Letter 'i'))       (False, textInputs)
      = (True, textInputs)
    progressOuter (KeyPress (Special ArrLeft))  (insert, textInputs)
      = (insert, maybeApply moveLeft textInputs)
    progressOuter (KeyPress (Special ArrRight)) (insert, textInputs)
      = (insert, maybeApply moveRight textInputs)
    progressOuter _ state = state

    renderStuff (insertMode, inputs)
      = besides2 (renderShow insertMode) (besides $ focusAsList $ onFocused debugEnvelope $ mapFocus renderTextInput inputs)


type Focus a = ([a],a,[a])

focus (focus:more) = ([],focus,more)
getFocused (_,focus,_) = focus
setFocused x = onFocused (const x)
focusAsList (left, focus, right) = reverse left ++ [focus] ++ right

moveRight (left, focus, x:right) = Just (focus:left, x, right)
moveRight (left, focus, [])      = Nothing

moveLeft (x:left, focus, right) = Just (left, x, focus:right)
moveLeft ([], focus, right)     = Nothing

onFocused f (left, focus, right) = (left, f focus, right)
mapFocus f (left, focus, right) = (map f left, f focus, map f right)


main = runGtkZero modeTextInputWidgetTest
