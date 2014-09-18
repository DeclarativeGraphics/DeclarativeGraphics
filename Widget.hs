module Widget where

import Data.Maybe
import Data.Either
import Data.List (replicate, partition)

import GtkUtils

import Graphics.Declarative.Form

import Control.Monad (liftM, liftM2)

import Colors
import Utils
import TextInput
import KeyboardInput


type StepFunc event value = event -> Maybe (Widget event value)

data Widget event value
  = Widget { stepWidget    :: event -> Maybe (Widget event value)
           , valueOfWidget :: value
           }

mapValueOfWidget :: (a -> b) -> Widget event a -> Widget event b
mapValueOfWidget f (Widget step value)
  = Widget (liftM (mapValueOfWidget f) . step) (f value)

preprocessEventsToWidget :: (a -> b) -> Widget b value -> Widget a value
preprocessEventsToWidget f (Widget step value)
  = Widget (liftM (preprocessEventsToWidget f) . step . f) value

runGTKWidget :: Widget GtkEvent Form -> IO ()
runGTKWidget widget = runGTK widget step valueOfWidget
  where
    step event widget
      = stepWidget widget event `orElse` widget
