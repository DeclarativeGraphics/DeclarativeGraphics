{-# LANGUAGE NoMonomorphismRestriction, TupleSections, ExistentialQuantification #-}
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


ourTextStyle = defaultTextStyle { fontSize = 23 }

renderShow :: (Show s) => s -> Form
renderShow = renderString . show

renderString :: String -> Form
renderString = text ourTextStyle

besides ls = centered $ groupBy toRight ls
besides2 x y = centered $ groupBy toRight [x, y]

renderTextInput :: TextInput -> Form
renderTextInput (l,r) = centered <| groupBy toRight [centered leftText, cursor, centered rightText]
  where
    leftText  = text ourTextStyle (reverse l)
    rightText = text ourTextStyle r
    cursor = rectangle 1 27 |> filled black |> flip withEnvelope emptyEnvelope

interpretTextInput :: GtkEvent -> (TextInput -> Maybe TextInput)
interpretTextInput (KeyPress key) = case key of
  Letter c           -> Just . textInputInsert c
  Special ArrLeft    -> textInputMoveLeft
  Special ArrRight   -> textInputMoveRight
  Special Backspace  -> textInputDelete
  _                  -> const Nothing
interpretTextInput _ = const Nothing

type StepFunc state = GtkEvent -> state -> Maybe state
type RenderFunc state = state -> Form

data Widget = forall state . Widget (StepFunc state) state (RenderFunc state)

textInputWidget init = Widget interpretTextInput (toTextInput init) renderTextInput

stepWidget :: GtkEvent -> Widget -> Maybe Widget
stepWidget event (Widget step state render)
  = do newState <- step event state
       return (Widget step newState render)

renderWidget :: Widget -> Form
renderWidget (Widget step state render) = render state

modeWidget widget
  = Widget step (False, widget) render
  where
    step event (False, widget) = progressOuter event (False, widget)
    step event (True,  widget) = case progressInner event widget of
      Just newWidget -> Just (True, newWidget)
      Nothing        -> progressOuter event (True, widget)

    progressInner = stepWidget

    progressOuter (KeyPress (Letter 'i'))       (False, widget)
      = Just (True, widget)
    progressOuter (KeyPress (Special Escape))   (_,      widget)
      = Just (False, widget)
    progressOuter _ state = Nothing

    debugEnv color form = (outlined (solid color) . fromEnvelope . fEnvelope $ form)
                          `atop` form

    render (True, widget) = debugEnv green (renderWidget widget)
    render (False, widget) = debugEnv grey (renderWidget widget)
      
modeTextInputWidgetTest
  = fuckingBigStuff >>^ accum (False, focus initialWidgets)
                    ^>> renderStuff
  where
    initialWidgets :: [Widget]
    initialWidgets
      = modeWidget (textInputWidget "Pihmel")
      : map textInputWidget ["foo", "bar", "spam", "eggs"]

    fuckingBigStuff :: GtkEvent
                    -> (Bool, Focus Widget)
                    -> (Bool, Focus Widget)
    fuckingBigStuff event (False, textInputs) = progressOuter event (False, textInputs)
    fuckingBigStuff event (True, textInputs) = case progressInner event textInputs of
      Just newState -> (True, newState)
      Nothing       -> progressOuter event (True, textInputs)

    progressInner :: GtkEvent
                  -> Focus Widget
                  -> Maybe (Focus Widget)
    progressInner event focus = do newWidget <- stepWidget event focusedWidget
                                   return $ setFocused newWidget focus
      where focusedWidget = getFocused focus

    progressOuter :: GtkEvent
                  -> (Bool, Focus Widget)
                  -> (Bool, Focus Widget)
    progressOuter (KeyPress (Letter 'i'))       (False, textInputs)
      = (True, textInputs)
    progressOuter (KeyPress (Special ArrLeft))  (insert, textInputs)
      = (insert, maybeApply moveLeft textInputs)
    progressOuter (KeyPress (Special ArrRight)) (insert, textInputs)
      = (insert, maybeApply moveRight textInputs)
    progressOuter (KeyPress (Special Escape))   (_,      textInputs)
      = (False, textInputs)
    progressOuter _ state = state

    renderStuff :: (Bool, Focus Widget)
                -> Form
    renderStuff (insertMode, inputs)
      = besides2 (renderShow insertMode)
                 (besides $ focusAsList $ onFocused debugEnvelope $ mapFocus (pad . renderWidget) inputs)
    pad = padded 15


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
