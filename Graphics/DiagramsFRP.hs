module Graphics.DiagramsFRP where

import Diagrams.Prelude
import Graphics.GtkFRP
import Diagrams.Backend.Cairo
import Diagrams.Backend.Gtk
import qualified Graphics.UI.Gtk as Gtk


diagramsFRPWidget :: Gtk.DrawingArea -> GtkFRP (Diagram Cairo R2) -> IO ()
diagramsFRPWidget = frpWidget renderFRPDiagram
  where renderFRPDiagram canvas diagram = do drawingArea <- Gtk.widgetGetDrawWindow canvas
                                             renderToGtk drawingArea diagram
