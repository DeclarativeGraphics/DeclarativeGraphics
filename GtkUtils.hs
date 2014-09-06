module GtkUtils where

import Control.Applicative
import Control.Monad
import Data.IORef
import Data.Maybe

import qualified Graphics.UI.Gtk as G
import Graphics.UI.Gtk (AttrOp (..) -- for :=
                       ,on
                       )
import qualified Graphics.Rendering.Cairo as C
import qualified Graphics.UI.Gtk.Gdk.EventM as E
import Graphics.Rendering.Cairo (liftIO)

import Graphics.Declarative.Form

import KeyboardInput


data GtkEvent = Expose
              | KeyPress KeyboardInput
              deriving (Show, Eq)


runGTK = runGTKat (0.5, 0.5)

runGTKat :: (Double, Double)
         -> state
         -> (GtkEvent -> state -> state)
         -> (state -> Form)
         -> IO ()
runGTKat origin state step formOf = gtkBoilerplate $ \canvas -> do
  stateRef <- newIORef state

  let processEvent :: Maybe GtkEvent -> E.EventM any ()
      processEvent (Just event) = liftIO $ do
        putStrLn $ "Got event " ++ show event
        state <- readIORef stateRef
        let newstate = step event state
        writeIORef stateRef newstate
        renderForm canvas origin (formOf newstate)
      processEvent Nothing = return ()

      gtkProcessEvent :: E.EventM i (Maybe GtkEvent) -> E.EventM i Bool
      gtkProcessEvent f = f >>= processEvent >> E.eventSent

  canvas `on` G.exposeEvent   $ gtkProcessEvent handleExpose
  canvas `on` G.keyPressEvent $ gtkProcessEvent handleKeyPress


handleExpose :: E.EventM E.EExpose (Maybe GtkEvent)
handleExpose = return (Just Expose)


handleKeyPress :: E.EventM E.EKey (Maybe GtkEvent)
handleKeyPress = do
  key <- E.eventKeyVal
  return $ KeyPress <$> keyboardInputFromGdk key


drawForm :: (Double, Double) -> Form -> Double -> Double -> C.Render ()
drawForm (originx,originy) form w h = do
  fDraw $ moved (originx * w, originy * h) $ form


renderForm :: G.WidgetClass w => w -> (Double, Double) -> Form -> IO ()
renderForm canvas origin form = renderDoubleBuffered canvas (drawForm origin form)



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
  G.widgetModifyBg canvas G.StateNormal gtkWhite
  G.widgetShowAll window

  G.onDestroy window G.mainQuit

  f canvas

  G.mainGUI 


gtkWhite = G.Color 65535 65535 65535


render :: G.WidgetClass w => w -> (Double -> Double -> C.Render ()) -> IO ()
render canvas renderF = do
  (w, h) <- G.widgetGetSize canvas
  drawWin <- G.widgetGetDrawWindow canvas
  G.renderWithDrawable drawWin (renderF (fromIntegral w) (fromIntegral h))


renderDoubleBuffered canvas renderF = render canvas renderF'
  where
    renderF' w h = do
      C.pushGroup
      delete w h 
      renderF w h
      C.popGroupToSource
      C.paint

    delete w h = do
      C.setSourceRGB 1 1 1
      C.rectangle 0 0 w h
      C.fill
