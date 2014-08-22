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

--- FRPZERO Ext

choice :: (statel -> stater -> state)
       -> State eventl statel
       -> State eventr stater
       -> State (Either eventl eventr) state
choice merge left right = State sf (merge (state left) (state right))
  where
    sf (Left  eventl) = choice merge (step eventl left) right
    sf (Right eventr) = choice merge left (step eventr right)

switch :: State event (Either (State event state) state)
       -> State event state
switch node = case state node of
                Left newNode -> newNode
                Right newState -> State sf newState
  where
    sf event = switch (step event node)

{----- SUSPENDED -----

--- FRPZEROCONNECTORS

split = choice

onLeft f split mrg l r = split mrg (f l) r
onRight f split mrg l r = split mrg l (f r)

merge f split = split f id id

close state = (state |>)

----------------------}


--- TESTING Prelude

instance Show (a -> b) where show _ = "<function>" -- hack

holdLast = (const . Just) >>^ accum Nothing

hold init = const >>^ accum init

besides2 x y = centered $ groupBy toRight [x, y]

renderString :: String -> Form
renderString = text defaultTextStyle

renderShow :: (Show s) => s -> Form
renderShow = renderString . show

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

dropAllEvents :: event -> Maybe x
dropAllEvents event = Nothing

--- Testing

constWidget form = \ leftSide processRight merge ->
  interpreterToDecider dropAllEvents
    >>^ choice merge leftSide (processRight (constant form))

lastEventWidget = \ leftSide processRight merge ->
  interpreterToDecider takeAllEvents
    >>^ choice merge leftSide (showLastEvent |> processRight)

counterWidget = \ leftSide processRight merge ->
  parallel merge leftSide (const (+ 1) >>^ accum (0 :: Int) ^>> renderShow |> processRight)

{-
modeWidget widget = \ leftSide processRight merge ->
  interpreterToDecider interpretModeInput
    >>^ 
-}

testSwitch w1 w2 leftSide processRight merge =
  switch $
    interpretInput
     >>^ choice ($) (hold Right) (w1 leftSide processRight merge)
  where
    interpretInput (KeyPress key) = case key of
      Special Escape -> Left (const (Left (testSwitch w2 w1 leftSide processRight merge)))
      _              -> Right (KeyPress key)
    interpretInput event = Right event

debugWidget widget = \ leftSide processRight merge ->
  interpreterToDecider takeAllEvents
    >>^ choice merge leftSide (widget showLastEvent id besides2 |> processRight)

system widget = widget ignore id (\ l r -> r)
systemDebug widget = widget showLastEvent id besides2
  
main = runGtkZero $ systemDebug $ testSwitch (constWidget (renderString "new Widget!"))
                                             counterWidget
