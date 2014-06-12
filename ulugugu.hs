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


groupBy2 direction x y = groupBy direction [x,y]

focusWrapper widget = loopFold False inner
  where
    inner = proc (event,focussed) -> do
      (form, widgetUnusedEvents) <- widget <<< merge keepWhen -< (focussed, event)

      bypassedEvents <- merge dropWhen -< (focussed, event)

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

      lastEvent <- holdLast Nothing <<< mapEvents Just -< event
      lastEventText <- mapBehavior (maybe emptyForm (text defaultTextStyle . show)) -< lastEvent

      widgetUnused <- holdLast Nothing <<< mapEvents Just -< unusedEvents
      widgetUnusedText <- mapBehavior (maybe emptyForm (text defaultTextStyle . show)) -< widgetUnused

      focusText <- mapBehavior (text defaultTextStyle . show) -< focussed

      renderedWidget <- merge (onBehavior2 (\ form focussed -> let transf = if focussed then debugEnvelope else id in transf form)) -< (form, focussed)
      img <- merge (onBehavior2 $ groupBy2 toRight) -< (renderedWidget, lastEventText)
      img' <- merge (onBehavior2 $ groupBy2 toRight) -< (img, focusText)
      img'' <- merge (onBehavior2 $ groupBy2 toRight) -< (img', widgetUnusedText)
      returnA -< (img'', focusEvents)


main = runGTK <| focusWrapper z
