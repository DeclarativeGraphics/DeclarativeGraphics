module DiagramsFRPMain where

import Diagrams.Prelude
import Diagrams.Example.Logo
import Diagrams.Backend.Cairo
import Graphics.FRP
import Graphics.GtkFRP
import Graphics.GtkFRPMain
import Graphics.DiagramsFRP

frpsys = Constant (text "Hallo" # scale 20)

main = runGtkFRP diagramsFRPWidget frpsys
