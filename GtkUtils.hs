module GtkUtils where

import Data.IORef
import Control.Monad
import Data.Maybe

import qualified Graphics.UI.Gtk as G
import Graphics.UI.Gtk (AttrOp (..) -- for :=
                       ,on
                       )
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.UI.Gtk.Gdk.EventM as E
import Graphics.Rendering.Cairo (liftIO)

import Graphics.Declarative.Form

import Automaton


data GtkEvent = Expose
              | KeyPress G.KeyVal
              deriving (Show)


type GtkFRP = Automaton (Event GtkEvent) (Behavior Form)


runGTK :: GtkFRP -> IO ()
runGTK automaton = gtkBoilerplate $ \canvas -> do
  automatonRef <- newIORef automaton

  let frpProcessEvent :: GtkEvent -> E.EventM any ()
      frpProcessEvent frpEvent = liftIO $ do
        automaton <- readIORef automatonRef
        let (newAutomaton, formBehavior) = stepEvent automaton frpEvent
        writeIORef automatonRef newAutomaton
        putStrLn $ "Got event " ++ show frpEvent
        renderForm canvas (behaviorValue formBehavior)

      processEvent :: E.EventM i GtkEvent -> E.EventM i Bool
      processEvent f = f >>= frpProcessEvent >> E.eventSent

  canvas `on` G.exposeEvent   $ processEvent handleExpose
  canvas `on` G.keyPressEvent $ processEvent handleKeyPress


handleExpose :: E.EventM E.EExpose GtkEvent
handleExpose = return Expose


handleKeyPress :: E.EventM E.EKey GtkEvent
handleKeyPress = do
  key <- E.eventKeyVal
  return (KeyPress key)


drawForm :: Form -> Double -> Double -> C.Render ()
drawForm form w h = do
  fDraw $ moved (w / 2, h / 2) $ centered $ form


renderForm :: G.WidgetClass w => w -> Form -> IO ()
renderForm canvas form = render canvas (drawForm form)



--- GTK boilerplate ---
gtkBoilerplate f = do
  G.initGUI

  window <- G.windowNew
  screen <- G.windowGetScreen window
  w <- G.screenGetWidth screen
  h <- G.screenGetHeight screen
  G.set window [G.windowDefaultWidth   := (ceiling $ 0.7 * fromIntegral w)
               ,G.windowDefaultHeight  := (ceiling $ 0.7 * fromIntegral h)
               ,G.windowWindowPosition := G.WinPosCenter
               ]

  canvas <- G.drawingAreaNew
  G.containerAdd window canvas

  G.set canvas [G.widgetCanFocus := True]
  G.widgetModifyBg canvas G.StateNormal white
  G.widgetShowAll window

  G.onDestroy window G.mainQuit

  f canvas

  G.mainGUI 


white = G.Color 65535 65535 65535


render :: G.WidgetClass w => w -> (Double -> Double -> C.Render ()) -> IO ()
render canvas renderF = do
  (w, h) <- G.widgetGetSize canvas
  drawWin <- G.widgetGetDrawWindow canvas
  G.renderWithDrawable drawWin (renderF (fromIntegral w) (fromIntegral h))
