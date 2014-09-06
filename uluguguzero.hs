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

choice :: (statel -> stater -> state)
       -> State eventl statel
       -> State eventr stater
       -> State (Either eventl eventr) state
choice merge left right = State sf (merge (state left) (state right))
  where
    sf (Left  eventl) = choice merge (step eventl left) right
    sf (Right eventr) = choice merge left (step eventr right)

stuff = interpreterToDecider interpretModeInput
          >>^ choice (const id) ignore (accum False ^>> renderShow)
          -- >>^ choice besides2 showLastEvent (accum False ^>> renderShow)
  where
    interpretModeInput (KeyPress key) = case key of
      Letter 'i'     -> Just $ const True
      Special Escape -> Just $ const False
      _              -> Nothing
    interpretModeInput _ = Nothing


--- INTERESTING STUFF ---

--- Prelude

ignore :: State event ()
ignore = constant ()

interpreterToDecider :: (a -> Maybe b) -> a -> Either a b
interpreterToDecider f event = case f event of
  Just output  -> Right output
  Nothing      -> Left event

showLastEvent :: Show event => State event Form
showLastEvent = holdLast ^>> maybe (renderString "No Events yet") renderShow

takeAllEvents :: event -> Maybe event
takeAllEvents event = Just event

dropAllEvents :: event -> Maybe event
dropAllEvents event = Nothing


--- Stages of increasing complexity
--
-- widget part is the widget itself
-- system part is the managing stuff, which takes the outermost widget
--                and feeds the relevant stuff (normally only the output
--                Form, in this case for debugging also the last Event
--                which the widget didn't process) to the `real' system

-- widget which only shows the Form `x' and ignores input
--   ideally: system (constWidget x)
constWidgetTest x
  = interpreterToDecider dropAllEvents
 -- ~~~~~~~~~~~~~~~~~~~~ ^^-widget--^^
      >>^ choice besides2 showLastEvent (constant x)
   -- >>^ choice takeRight  ignore      (constant x)
      --  ^^^---------system--------^^^ ^^-widget-^^

-- widget which shows last Event and so consumes all input
--   ideally: system (lastEventWidget)
lastEventWidgetTest
  = interpreterToDecider takeAllEvents
 -- ~~~~~~~~~~~~~~~~~~~~ ^^-widget--^^
      >>^ choice besides2 showLastEvent showLastEvent
      --  ^^^---------system--------^^^ ^^--widget-^^

-- widget which shows inner widget's (constWidget "inner") Form
--   and unused input, similar to the system, but as a widget
--   itself and so gives the system an event stream (no events,
--   uses all to give them to the inner widget and the inner
--   widget's unused events will be shown; so they are also
--   consumed) and a resulting Form
--
--  inner  - inner widget (the inner widget, in this case ~= `constWidget "inner"')
--  outer  - outer widget (debugWidget itself)
--  outer* - outer widget containing the inner widget
--  
--  ideally: system (debugWidget (constWidget (renderString "inner")))
debugWidgetTest1
  = interpreterToDecider takeAllEvents
 -- ~~~~~~~~~~~~~~~~~~~~ ^^--outer--^^
      >>^ choice besides2 showLastEvent debugWidget
      --  ^^^---------system--------^^^ ^^-outer*-^
  where
    debugWidget
      = interpreterToDecider dropAllEvents
     -- ~~~~~~~~~~~~~~~~~~~^ ^^--inner--^^
          >>^ choice besides2 showLastEvent (constant (renderString "inner"))
          --  ^^^--outer (debugWidget)--^^^ ^^-inner (constWidget "inner")-^^


-- widget which shows inner widget's (lastEventWidget) Form
--   and unused input, similar to the system, but as a widget
--   itself and so gives the system an event stream (no events,
--   uses all to give them to the inner widget and the inner
--   widget's unused events will be shown; so they are also
--   consumed) and a resulting Form
--
--  inner  - inner widget (the inner widget, in this case ~= `lastEventWidget')
--  outer  - outer widget (debugWidget itself)
--  outer* - outer widget containing the inner widget
--  
--  ideally: system (debugWidget lastEventWidget)
debugWidgetTest2
  = interpreterToDecider takeAllEvents
 -- ~~~~~~~~~~~~~~~~~~~~ ^^--outer--^^
      >>^ choice besides2 showLastEvent debugWidget
      --  ^^^---------system--------^^^ ^^-outer*-^
  where
    debugWidget
      = interpreterToDecider takeAllEvents
     -- ~~~~~~~~~~~~~~~~~~~^ ^^--inner--^^
          >>^ choice besides2 showLastEvent showLastEvent
          --  ^^^--outer (debugWidget)--^^^ ^^^-inner-^^^

textInputWidgetTest
  = interpreterToDecider interpretTextInput
      >>^ choice besides2 showLastEvent textInputState
  where
    interpretTextInput (KeyPress key) = case key of
      Letter c           -> Just $ textInputInsert c
      Special ArrLeft    -> Just $ maybeApply textInputMoveLeft
      Special ArrRight   -> Just $ maybeApply textInputMoveRight
      Special Backspace  -> Just $ maybeApply textInputDelete
      _                  -> Nothing
    interpretTextInput _ = Nothing

    textInputState = accum emptyTextInput ^>> renderTextInput
