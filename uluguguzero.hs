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

changingRect = foldp (const tail) (cycle allColorsRandomOrder) |> after (drawRect . head)
  where
    drawRect color = rectangle 200 200 |> filled color

holdLast = foldp (const . Just) Nothing

lastEvent :: (Show event) => State event Form
lastEvent = holdLast |> after (originatedRel (0.5,0.5) . maybe emptyForm (text defaultTextStyle . show))

textWidget = foldp (flip interpretInput) emptyTextInput |> after renderTextInput
  where
    interpretInput :: TextInput -> GtkEvent -> TextInput
    interpretInput state (KeyPress key) = snd <| case key of
      Letter c           -> (False, textInputInsert c state)
      Special ArrLeft    -> (False, maybeApply textInputMoveLeft state)
      Special ArrRight   -> (False, maybeApply textInputMoveRight state)
      Special Backspace  -> (False, maybeApply textInputDelete state)
      _                  -> (True, state)
    interpretInput state _ = snd (True, state)

    renderTextInput :: TextInput -> Form
    renderTextInput (l,r) = groupBy toRight [centered <| text defaultTextStyle (reverse l),
                                             rectangle 1 20 |> filled black |> modifiedEnvelope (\ (Envelope l t r b) -> Envelope 0 t 0 b),
                                             centered <| text defaultTextStyle r]
                              -- |> originatedRel (0.5,0.5)

main = runGtkZero (merge atop (after debugEnvelope textWidget) (after debugEnvelope changingRect))
