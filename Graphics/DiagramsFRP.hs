module Graphics.DiagramsFRP where

import Diagrams.Prelude
import Graphics.GtkFRP
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Diagrams.Backend.Gtk
import qualified Graphics.Rendering.Cairo as CG
import qualified Graphics.UI.Gtk as Gtk


diagramsFRPWidget :: Monoid' m =>
                     Gtk.DrawingArea
                     -> GtkFRP (QDiagram Cairo R2 m)
                     -> IO ()
diagramsFRPWidget = frpWidget renderDiagram


-- TODO https://github.com/diagrams/diagrams-gtk/pull/1
renderDiagram :: Monoid' m => Gtk.DrawingArea -> QDiagram Cairo R2 m -> IO ()
renderDiagram canvas diagram = do
  (w,h) <- Gtk.widgetGetSize canvas
  dw <- Gtk.widgetGetDrawWindow canvas
  let r = snd $ renderDia Cairo
                  (CairoOptions
                     { _cairoFileName     = ""
                     , _cairoSizeSpec     = Absolute
                     , _cairoOutputType   = RenderOnly
                     , _cairoBypassAdjust = True
                     }
                  )
                  diagram
  Gtk.renderWithDrawable dw (doubleBuffer $ delete w h >> r)
 where
   delete w h = do
     CG.setSourceRGB 1 1 1
     CG.rectangle 0 0 (fromIntegral w) (fromIntegral h)
     CG.fill
   doubleBuffer render
     = CG.pushGroup >> render >> CG.popGroupToSource >> CG.paint
