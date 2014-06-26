module Main where

import Data.Maybe

import FRPZero
import FRPZeroGtk
import GtkUtils

import Graphics.Declarative.Envelope
import Graphics.Declarative.Shape
import Graphics.Declarative.Form
import Graphics.Declarative.Combinators

import Colors
import Utils
import TextInput
import KeyboardInput

changingRect = const tail >>^ accum (cycle colors) ^>> (drawRect . head)
  where
    colors = [red, blue, green, yellow, brown, purple, grey]
    drawRect color = rectangle 200 200 |> filled color

holdLast = (const . Just) >>^ accum Nothing

lastEvent :: (Show event) => State event Form
lastEvent = holdLast |> after (centered . maybe emptyForm renderShow)

renderShow :: (Show s) => s -> Form
renderShow = text defaultTextStyle . show

textWidget = interpretTextInput >>^ accum emptyTextInput ^>> renderTextInput
  where
    interpretTextInput :: GtkEvent -> (TextInput -> TextInput)
    interpretTextInput (KeyPress key) = case key of
      Letter c           -> textInputInsert c
      Special ArrLeft    -> maybeApply textInputMoveLeft
      Special ArrRight   -> maybeApply textInputMoveRight
      Special Backspace  -> maybeApply textInputDelete
      _                  -> id
    interpretTextInput _ = id

renderTextInput :: TextInput -> Form
renderTextInput (l,r) = centered <| groupBy toRight [centered leftText, cursor, centered rightText]
  where
    leftText  = text defaultTextStyle (reverse l)
    rightText = text defaultTextStyle r
    cursor = rectangle 1 20 |> filled black |> flip withEnvelope emptyEnvelope

main = runGtkZero (textWidget ^>> debugEnvelope) -- (merge atop (after debugEnvelope textWidget) (after debugEnvelope changingRect))
