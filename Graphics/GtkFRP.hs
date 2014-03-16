module Graphics.GtkFRP where

import Control.Monad
import Data.IORef

import Graphics.FRP
import Graphics.FRPUtils

import Graphics.UI.Gtk hiding (Point)
import qualified Graphics.Rendering.Cairo as CG
import Graphics.Rendering.Cairo (liftIO)


data GtkEvent = MouseMove (Point Double)
              | MousePress MouseButton
              | MouseRelease MouseButton
              | KeyPress KeyVal
              | KeyRelease KeyVal
              | Resize (Int,Int)
              deriving (Eq,Show)

type GtkFRP state = FRP (Event GtkEvent) state

type Point a = (a,a)
type RGB a = (a,a,a)


frpWidget :: (DrawingArea -> state -> IO ()) -- ^ rendering function
          -> DrawingArea -- ^ the drawable
          -> GtkFRP state -- ^ the FRP system
          -> IO ()
frpWidget renderFRPState canvas frpsys = do
      frp <- newIORef (getState frpsys, frpsys)

      let draw = renderFRPState canvas

      canvas `on` exposeEvent $ processEvent $ liftIO $ do
        draw =<< getFRPState frp

      canvas `on` motionNotifyEvent $ processEvent $ do
        coord <- eventCoordinates
        liftIO $ draw =<< event frp (MouseMove coord)
        eventRequestMotions
      
      canvas `on` buttonPressEvent $ processEvent $ do
        button <- eventButton
        liftIO $ draw =<< event frp (MousePress button)

      canvas `on` buttonReleaseEvent $ processEvent $ do
        button <- eventButton
        liftIO $ draw =<< event frp (MouseRelease button)

      canvas `on` keyPressEvent $ processEvent $ do
        key <- eventKeyVal
        liftIO $ draw =<< event frp (KeyPress key)

      canvas `on` keyReleaseEvent $ processEvent $ do
        key <- eventKeyVal
        liftIO $ draw =<< event frp (KeyRelease key)

      canvas `on` configureEvent $ processEvent $ do
        size <- eventSize
        liftIO $ draw =<< event frp (Resize size)

      return ()
  where
    processEvent :: Monad m => m a -> m Bool
    processEvent = (>> return True)
    event frpref ev = do
      (_,actfrp) <- readIORef frpref
      let newfrpsys = run actfrp (Event ev)
      writeIORef frpref newfrpsys
      return (frpstate newfrpsys)
    frpstate (state,_) = state
    getFRPState = liftM frpstate . readIORef


--------- FRP ---------

resizeEvent :: FRP (Event GtkEvent) (Event (Int,Int))
resizeEvent = eventExtract getResizeSize
  where
    getResizeSize (Resize size) = Just size
    getResizeSize _ = Nothing

size :: FRP (Event (Int,Int)) (Int,Int)
size = hold (0,0)



---- MOUSE ----

mouseMove :: FRP (Event GtkEvent) (Event (Point Double))
mouseMove = eventExtract getMouseMovePosition
  where
    getMouseMovePosition (MouseMove pos) = Just pos
    getMouseMovePosition _ = Nothing

mousePress :: FRP (Event GtkEvent) (Event MouseButton)
mousePress = eventExtract getMousePressButton
  where
    getMousePressButton (MousePress btn) = Just btn
    getMousePressButton _ = Nothing

mouseRelease :: FRP (Event GtkEvent) (Event MouseButton)
mouseRelease = eventExtract getMouseReleaseButton
  where
    getMouseReleaseButton (MouseRelease btn) = Just btn
    getMouseReleaseButton _ = Nothing

mousePos :: FRP (Event GtkEvent) (Point Double)
mousePos = hold (0,0) <<< mouseMove



---- KEY ----

keyPress :: FRP (Event GtkEvent) (Event KeyVal)
keyPress = eventExtract getKeyPressKey
  where
    getKeyPressKey (KeyPress key) = Just key
    getKeyPressKey _ = Nothing

keyRelease :: FRP (Event GtkEvent) (Event KeyVal)
keyRelease = eventExtract getKeyReleaseKey
  where
    getKeyReleaseKey (KeyRelease key) = Just key
    getKeyReleaseKey _ = Nothing
