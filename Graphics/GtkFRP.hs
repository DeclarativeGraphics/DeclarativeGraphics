module Graphics.GtkFRP where

import Control.Monad
import Data.IORef

import Graphics.FRP
import Graphics.FRPUtils
import Graphics.WidgetFRP

import Graphics.UI.Gtk hiding (Point)
import qualified Graphics.Rendering.Cairo as CG
import Graphics.Rendering.Cairo (liftIO)


frpWidget :: (DrawingArea -> state -> IO ()) -- ^ rendering function
          -> DrawingArea -- ^ the drawable
          -> WidgetFRP state -- ^ the FRP system
          -> IO ()
frpWidget renderFRPState canvas frpsys = do
      frp <- newIORef (getState frpsys, frpsys)

      canvas `on` exposeEvent $ processGtkEvent $ liftIO $ do
        draw =<< getFRPState frp

      canvas `on` motionNotifyEvent $ processGtkEvent $ do
        coord <- eventCoordinates
        liftIO $ processFRPEvent frp MouseMove coord
        eventRequestMotions
      
      canvas `on` buttonPressEvent $ processGtkEvent $ do
        button <- eventButton
        liftIO $ maybe (return ()) (processFRPEvent frp MousePress) (convertMouseButton button)

      canvas `on` buttonReleaseEvent $ processGtkEvent $ do
        button <- eventButton
        liftIO $ maybe (return ()) (processFRPEvent frp MouseRelease) (convertMouseButton button)

      canvas `on` keyPressEvent $ processGtkEvent $ do
        key <- eventKeyVal
        liftIO $ maybe (return ()) (processFRPEvent frp KeyPress) (convertKey key)

      canvas `on` keyReleaseEvent $ processGtkEvent $ do
        key <- eventKeyVal
        liftIO $ maybe (return ()) (processFRPEvent frp KeyRelease) (convertKey key)

      canvas `on` configureEvent $ processGtkEvent $ do
        size <- eventSize
        liftIO $ processFRPEvent frp Resize size

      return ()
  where
    draw = renderFRPState canvas . behaviorValue

    processGtkEvent :: Monad m => m a -> m Bool
    processGtkEvent = (>> return True)

    processFRPEvent frp eventType eventValue
      = draw =<< event frp (eventType eventValue)

    event frpref ev = do
      (_,actfrp) <- readIORef frpref
      let newfrpsys = run actfrp (Event ev)
      writeIORef frpref newfrpsys
      return (frpstate newfrpsys)

    frpstate (state,_) = state
    getFRPState = liftM frpstate . readIORef


convertMouseButton button = case button of
  LeftButton -> Just MBLeft
  RightButton -> Just MBRight
  MiddleButton -> Just MBMiddle
  _ -> Nothing


convertKey key = case keyName key of
  "Return" -> Just Return
  "space"  -> Just Space
  [c]      -> Just (Letter c)
  _        -> Nothing
