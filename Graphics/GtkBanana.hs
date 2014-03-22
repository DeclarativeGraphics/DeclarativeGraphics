module Graphics.GtkBanana where

import Control.Monad
import Reactive.Banana
import Reactive.Banana.Frameworks
import Graphics.UI.Gtk hiding (Point)
import Graphics.GtkFRPMain


data Event = MouseMove (Double,Double) | Expose


main = runGtkFRP gtkFRP


gtkFRP :: DrawingArea -> IO ()
gtkFRP canvas = do
  let frpDescription :: Frameworks t => Moment t ()
      frpDescription = do

        expose      <- registerGtkEvent canvas exposeEvent $ do
                        return Expose

        mouseMove   <- registerGtkEvent' canvas motionNotifyEvent (\fireEvent -> do
                        coord <- eventCoordinates
                        liftIOGtk $ fireEvent (MouseMove coord)
                        eventRequestMotions)

        reactimate $ fmap (\Expose -> putStrLn "exposed") expose
        reactimate $ fmap (\(MouseMove (x,y)) -> print (x,y)) mouseMove

  actuate =<< compile frpDescription


-- | Listen to a specific GTK event. The 'gtkEventHandler' is called whenever
--   that GTK event occurs. It is passed a 'fireEvent' function which may be
--   used to fire a Banana event.
--
-- > someEventHandler fireEvent = do
-- >   ... do stuff in GTK's EventM monad ...
-- >   liftIOGtk $ fireEvent yourEventData
--
registerGtkEvent' canvas gtkEvent gtkEventHandler = do
  addHandler <- liftIOBanana $ do
    (addHandler, fireEvent) <- newAddHandler
    canvas `on` gtkEvent $ gtkEventHandler fireEvent >> return True
    return addHandler
  fromAddHandler addHandler


-- | Same as 'registerGtkEvent'' except that this is passed an 'extractEvent'
--   function instead that must return the Banana event data -- i.e. it doesn't
--   need to call 'fireEvent' itself.
--
-- > someEventHandler = do
-- >   ... do stuff in GTK's EventM monad ...
-- >   return yourEventData
--
registerGtkEvent c e extractEvent = registerGtkEvent' c e simpleEventHandler
  where simpleEventHandler fireEvent = extractEvent >>= liftIOGtk . fireEvent


-- | Lift stuff from 'IO' monad into 'Moment' monad
liftIOBanana :: Frameworks t => IO a -> Moment t a
liftIOBanana = liftIO

-- | Lift stuff from 'IO' monad into 'EventM' monad
liftIOGtk :: IO a -> EventM t a
liftIOGtk    = liftIO
