{-# LANGUAGE Arrows #-}
module FRP.Stdlib where

import Control.Arrow
import FRP.Base
import FRP.Arrow


partitionEvents :: FRP (Behavior Bool, Event a) (Event a, Event a)
partitionEvents = (&&&) (merge keepWhen) (merge dropWhen)


splitEvents :: FRP (Event (Either a b)) (Event a, Event b)
splitEvents = (&&&) (mapMaybeEvents maybeLeft) (mapMaybeEvents maybeRight)
  where
    maybeLeft (Left  x) = Just x
    maybeLeft (Right _) = Nothing

    maybeRight (Left  _) = Nothing
    maybeRight (Right x) = Just x


filterJusts :: FRP (Event (Maybe a)) (Event a)
filterJusts = mapMaybeEvents id


holdLast :: a -> FRP (Event a) (Behavior a)
holdLast init = foldEvents (\_ n -> n) init
