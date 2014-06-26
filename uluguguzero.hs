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

changingRect = foldp (const tail) (cycle colors) |> after (drawRect . head)
  where
    colors = [red, blue, green, yellow, brown, purple, grey]
    drawRect color = rectangle 200 200 |> filled color

holdLast = foldp (const . Just) Nothing

lastEvent :: (Show event) => State event Form
lastEvent = holdLast |> after (centered . maybe emptyForm renderShow)

renderShow :: (Show s) => s -> Form
renderShow = text defaultTextStyle . show

textWidget = before interpretInput (accum emptyTextInput) |> after renderTextInput
  where
    interpretInput :: GtkEvent -> (TextInput -> TextInput)
    interpretInput (KeyPress key) = case key of
      Letter c           -> textInputInsert c
      Special ArrLeft    -> maybeApply textInputMoveLeft
      Special ArrRight   -> maybeApply textInputMoveRight
      Special Backspace  -> maybeApply textInputDelete
      _                  -> id
    interpretInput _ = id

    renderTextInput :: TextInput -> Form
    renderTextInput (l,r) = groupBy toRight [centered <| text defaultTextStyle (reverse l),
                                             rectangle 1 20 |> filled black |> modifiedEnvelope (\ (Envelope l t r b) -> Envelope 0 t 0 b),
                                             centered <| text defaultTextStyle r]
                              |> centered

main = runGtkZero (after debugEnvelope textWidget) -- (merge atop (after debugEnvelope textWidget) (after debugEnvelope changingRect))
