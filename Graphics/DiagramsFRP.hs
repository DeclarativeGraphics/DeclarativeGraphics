module Graphics.DiagramsFRP where

import Diagrams.Prelude
import Graphics.GtkFRP
import Diagrams.Backend.Cairo
import Diagrams.Backend.Gtk
import qualified Graphics.UI.Gtk as Gtk


renderDiagram :: Gtk.DrawingArea -> Diagram Cairo R2 -> IO ()
renderDiagram canvas diagram = do drawingArea <- Gtk.widgetGetDrawWindow canvas
                                  renderToGtk drawingArea diagram
