module FRPZeroGtk where

import FRP
import GtkUtils
import FRPZero

import Control.Arrow

runGtkZero frpz = runGTK (foldEvents (\frpz event -> step event frpz) frpz >>> mapBehavior state)
