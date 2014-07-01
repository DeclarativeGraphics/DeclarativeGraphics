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

modeWidget :: (GtkEvent -> (state -> state)) -> state -> (state -> Form) -> State GtkEvent Form
modeWidget interpretInput init render
  = mergeF (.) interpretModeInput (insertModeAction . interpretInput)
      >>^ accum (False, init)
      ^>> renderWidget
  where
    renderWidget (insertMode, state) = showMode (render state)
      where showMode = if insertMode then id else debugEnvelope

    interpretModeInput :: GtkEvent -> ((Bool, state) -> (Bool, state))
    interpretModeInput (KeyPress key) = case key of
      Letter 'i'     -> onlyInMode False $ setInsertMode True
      Special Escape -> onlyInMode True  $ setInsertMode False
      _              -> id
    interpretModeInput _ = id

    setInsertMode :: Bool -> (Bool, state) -> (Bool, state)
    setInsertMode mode (_, state) = (mode, state)

    insertModeAction :: (state -> state) -> ((Bool, state) -> (Bool, state))
    insertModeAction = onlyInMode True . onState
    
    onState :: (state -> state) -> ((Bool, state) -> (Bool, state))
    onState f (mode, state) = (mode, f state)

    onlyInMode :: Bool -> ((Bool, state) -> (Bool, state)) -> ((Bool, state) -> (Bool, state))
    onlyInMode desiredMode f (mode, state) = maybeF (mode, state)
      where maybeF = if desiredMode == mode then f else id

onTuple f (x,y) = (f x, f y)

onFst, onSnd :: (a -> a) -> (a, a) -> (a, a)
onFst f (x,y) = (f x, y)
onSnd f (x,y) = (x, f y)

besides2 x y = centered $ groupBy toRight [x, y]

onTuple2 f = mergeF (,) (f . fst) (f . snd)

type OnTupleSideFunction a = (a -> a) -> (a,a) -> (a,a)

type Zustand = (Bool, OnTupleSideFunction Form, OnTupleSideFunction TextInput, (TextInput, TextInput))

multiModeWidget
  = mergeF ($) interpretModeInput (onFocused . interpretTextInput)
      >>^ accum widgetInit ^>> renderWidget
  where
    init = toTextInput "Haskell stinkt"
    widgetInit :: Zustand
    widgetInit = (False, onFst, onFst, (init, init))

    renderWidget :: Zustand -> Form
    renderWidget (insertMode, f0, f1, widgets)
      = showMode $ uncurry besides2 $ f0 debugEnvelope $ onTuple2 renderTextInput widgets
      where
        showMode = if insertMode then debugEnvelope else id


    interpretModeInput :: GtkEvent -> (Zustand -> Zustand) -> Zustand -> Zustand
    interpretModeInput (KeyPress key) = case key of
      Letter 'i'     -> onlyInMode False (setMode True)
      Special Escape -> onlyInMode True  (setMode False)
      Letter 'h'     -> onlyInMode False (setFocus onFst onFst)
      Letter 'l'     -> onlyInMode False (setFocus onSnd onSnd)
      _              -> onlyInMode False id
    interpretModeInput _ = onlyInMode False id

    setMode :: Bool -> (Zustand -> Zustand)
    setMode mode (_, f0, f1, widgets) = (mode, f0, f1, widgets)

    onlyInMode :: Bool -> (Zustand -> Zustand) -> (Zustand -> Zustand) -> (Zustand -> Zustand)
    onlyInMode desiredMode f g (mode, f0, f1, widgets)
      = (if desiredMode == mode then f else g) (mode, f0, f1, widgets)

    setFocus :: OnTupleSideFunction Form -> OnTupleSideFunction TextInput -> (Zustand -> Zustand)
    setFocus f0 f1 (mode, _, _, widgets) = (mode, f0, f1, widgets)

    onFocused :: (TextInput -> TextInput) -> (Zustand -> Zustand)
    onFocused f (mode, f0, f1, states) = (mode, f0, f1, f1 f states)


interpretTextInput' :: GtkEvent -> TextInput -> Maybe TextInput
interpretTextInput' (KeyPress key) textinput = case key of
  Letter c           -> Just $ textInputInsert c textinput
  Special ArrLeft    -> textInputMoveLeft textinput
  Special ArrRight   -> textInputMoveRight textinput
  Special Backspace  -> textInputDelete textinput
  _                  -> Nothing
interpretTextInput' _ textinput = Nothing

orElse :: Maybe a -> a -> a
orElse (Just x) _   = x
orElse Nothing  def = def

dropWhen :: State event Bool -> State event state -> State event state
dropWhen filter inner = State sf (state inner)
  where
    sf event = if state filter
                 then dropWhen (step event filter) inner
                 else dropWhen (step event filter) (step event inner)

stuff = dropWhen (not `after` mode) inner
  where
    inner = holdLast ^>> maybe emptyForm renderShow

    mode = interpretModeInput >>^ accum False

    interpretModeInput (KeyPress key) = case key of
      Letter 'i'     -> const True
      Special Escape -> const False
      _              -> id
    interpretModeInput _ = id

main = runGtkZero stuff
