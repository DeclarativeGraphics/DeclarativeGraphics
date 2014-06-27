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

hold init = const >>^ accum init

lastEvent :: (Show event) => State event Form
lastEvent = holdLast |> after (centered . maybe emptyForm renderShow)

renderShow :: (Show s) => s -> Form
renderShow = renderString . show

renderString :: String -> Form
renderString = text defaultTextStyle

textWidget = interpretTextInput >>^ accum emptyTextInput ^>> renderTextInput

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

inputMode = interpretInput >>^ maybeFilter (hold False)
  where
    interpretInput :: GtkEvent -> Maybe Bool
    interpretInput (KeyPress key) = case key of
      Letter 'i'     -> Just True
      Special Escape -> Just False
      _              -> Nothing
    interpretInput _ = Nothing

bla = fancystuff >>^ accum emptyTextInput ^>> renderTextInput
  where
    fancystuff = mergeF (.) interpretTextInput interpretModeInput --interpretTextInput 

    interpretModeInput :: GtkEvent -> TextInput -> TextInput
    interpretModeInput (KeyPress key) = case key of
      Letter 'i'     -> const $ toTextInput $ show True
      Special Escape -> const $ toTextInput $ show False
      _              -> id
    interpretModeInput _ = id
    
mergeF :: (a -> b -> c) -> (i -> a) -> (i -> b) -> (i -> c)
mergeF mrg f g = \ i -> mrg (f i) (g i)


modeInputWidget = mergeF (.) interpretModeInput (insertModeAction . interpretTextInput)
                    >>^ accum (False, emptyTextInput) ^>> renderWidget
  where
    renderWidget (insertMode, textinput) = showMode (renderTextInput textinput)
      where showMode = if insertMode then id else debugEnvelope

    interpretModeInput (KeyPress key) = case key of
      Letter 'i'     -> setInsertMode True
      Special Escape -> setInsertMode False
      _              -> id
    interpretModeInput _ = id

    setInsertMode mode (_, textinput) = (mode, textinput)

    insertModeAction f (False, textinput) = (False, textinput)
    insertModeAction f (True,  textinput) = (True,  f textinput)


vimModeInputWidget = mergeF (.) interpretModeInput (insertModeAction . interpretTextInput)
                       >>^ accum (False, emptyTextInput) ^>> renderWidget
  where
    renderWidget (insertMode, textinput) = showMode (renderTextInput textinput)
      where showMode = if insertMode then id else debugEnvelope

    interpretModeInput :: GtkEvent -> ((Bool, TextInput) -> (Bool, TextInput))
    interpretModeInput (KeyPress key) = case key of
      Letter 'i'     -> inMode False $ setInsertMode True
      Special Escape -> inMode True  $ setInsertMode False
      Letter 'h'     -> inMode False (onTextInput (maybeApply textInputMoveLeft))
      Letter 'l'     -> inMode False (onTextInput (maybeApply textInputMoveRight))
      _              -> id
    interpretModeInput _ = id

    setInsertMode :: Bool -> (Bool, TextInput) -> (Bool, TextInput)
    setInsertMode mode (_, textinput) = (mode, textinput)

    onTextInput :: (TextInput -> TextInput) -> ((Bool, TextInput) -> (Bool, TextInput))
    onTextInput f (mode, textinput) = (mode, f textinput)

    insertModeAction :: (TextInput -> TextInput) -> ((Bool, TextInput) -> (Bool, TextInput))
    insertModeAction = inMode True . onTextInput

    inMode :: Bool -> ((Bool, TextInput) -> (Bool, TextInput)) -> ((Bool, TextInput) -> (Bool, TextInput))
    inMode desiredMode f (mode, textinput) = maybeF (mode, textinput)
      where maybeF = if desiredMode == mode then f else id


main = runGtkZero vimModeInputWidget
