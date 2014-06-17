{-# LANGUAGE Arrows #-}
module Main where

import Control.Arrow
import Data.Maybe

import Graphics.Declarative.Envelope
import Graphics.Declarative.Shape
import Graphics.Declarative.Form
import Graphics.Declarative.Combinators
import GtkUtils

import Colors

import FRP
import TextInput
import KeyboardInput

import Utils

z = proc event -> do
  (textinputactions, unusedevents) <- splitEvents <<< mapEvents interpretInput -< event
  textinput <- accumEvents emptyTextInput -< textinputactions
  textinputRendered <- mapBehavior renderTextInput -< textinput

  returnA -< (textinputRendered, unusedevents)
 where
   interpretInput (KeyPress key) = case key of
     Letter c -> Left <| textInputInsert c
     Special ArrLeft    -> Left <| try textInputMoveLeft
     Special ArrRight   -> Left <| try textInputMoveRight
     Special Backspace  -> Left <| try textInputDelete
     _ -> Right (KeyPress key)
   interpretInput e = Right e

   try f x = fromMaybe x (f x)

   renderTextInput (l,r) = groupBy toRight [centered <| text defaultTextStyle (reverse l),
                                            rectangle 1 20 |> filled black |> modifiedEnvelope (\ (Envelope l t r b) -> Envelope 0 t 0 b),
                                            centered <| text defaultTextStyle r]


focusWrapper widget = loopFold False inner
  where
    inner = proc (event,focused) -> do
      (form, widgetUnusedEvents) <- widget <<< merge keepWhen -< (focused, event)

      bypassedEvents <- merge dropWhen -< (focused, event)

      (loseFocusEvents, wrapperUnused1)
        <- splitEvents <<< mapEvents (\gtkEvent -> case gtkEvent of KeyPress (Special Escape) -> Left False
                                                                    e -> Right e)
        -< widgetUnusedEvents

      (gainFocusEvents, wrapperUnused2)
        <- splitEvents <<< mapEvents (\gtkEvent -> case gtkEvent of KeyPress (Letter 'i') -> Left True
                                                                    e -> Right e)
        -< bypassedEvents

      unusedEvents <- merge mergeEvents -< (wrapperUnused1, wrapperUnused2)
      focusEvents <- merge mergeEvents -< (loseFocusEvents, gainFocusEvents)

      returnA -< ((form, unusedEvents), focusEvents)


debugFocusWrapper :: FRP (Event GtkEvent) (Behavior Form, Event GtkEvent) -> GtkFRP
debugFocusWrapper widget = debug <<< focusWrapper widget
  where
    debug = proc (form, unusedEvents) -> do
      unusedEventsText <- foldEvents' (text defaultTextStyle . show) (text defaultTextStyle "-") -< unusedEvents
      merge (mergeBehaviors render) -< (form, unusedEventsText)

    render form unusedEventsText = groupBy toBottom [form, unusedEventsText]


main = runGTK <| debugFocusWrapper z
