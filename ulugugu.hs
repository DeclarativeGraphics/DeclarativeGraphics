{-# LANGUAGE Arrows #-}
module Main where

import Control.Arrow

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


widget :: (state -> GtkEvent -> (Bool,state))
       -> (state -> Form)
       -> state
       -> FRP (Event GtkEvent) (Behavior Form, Event GtkEvent)
widget stepWidget renderWidget initWidget =
  eventPropagationWrapper (foldEvents (stepWidget . snd) (True, initWidget))
  >>> first (mapBehavior renderWidget)


eventPropagationWrapper :: FRP (Event a) (Behavior (Bool,b))
                        -> FRP (Event a) (Behavior b, Event a)
eventPropagationWrapper eventHandler = proc event -> do
  innerOutput <- eventHandler -< event
  (continuePropagation,output) <- unzipBehavior -< innerOutput
  propagatedEvents <- merge keepWhen -< (continuePropagation, event)
  returnA -< (output, propagatedEvents)


textWidget :: TextInput -> FRP (Event GtkEvent) (Behavior Form, Event GtkEvent)
textWidget = widget interpretInput renderTextInput
  where
    interpretInput :: TextInput -> GtkEvent -> (Bool,TextInput)
    interpretInput state (KeyPress key) = case key of
      Letter c           -> (False, textInputInsert c state)
      Special ArrLeft    -> (False, maybeApply textInputMoveLeft state)
      Special ArrRight   -> (False, maybeApply textInputMoveRight state)
      Special Backspace  -> (False, maybeApply textInputDelete state)
      _                  -> (True, state)
    interpretInput state _ = (True, state)

    renderTextInput :: TextInput -> Form
    renderTextInput (l,r) = groupBy toRight [centered <| text defaultTextStyle (reverse l),
                                             rectangle 1 20 |> filled black |> modifiedEnvelope (\ (Envelope l t r b) -> Envelope 0 t 0 b),
                                             centered <| text defaultTextStyle r]



focusWrapper widget = loopFold False $
  proc (event, focused) -> do
    (widgetEvents, bypassedEvents) <- partitionEvents -< (focused, event)

    (form, widgetUnusedEvents) <- widget -< widgetEvents

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


main = runGTK $ debugFocusWrapper $ textWidget emptyTextInput
