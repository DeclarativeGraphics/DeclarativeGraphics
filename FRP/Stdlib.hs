{-# LANGUAGE Arrows #-}
module FRP.Stdlib where

import Control.Arrow
import FRP.Base
import FRP.Arrow


-- | An FRP system that partitions events to the left- or right-hand side of
--   a 2-tuple, depending on the boolean value of a predicate behavior.
partitionEvents :: FRP (Behavior Bool, Event a) (Event a, Event a)
partitionEvents = (&&&) (merge keepWhen) (merge dropWhen)


-- | An FRP system that partitions events to the left- or right-hand side of
--   a 2-tuple, depending on if they are on the 'Left' or 'Right' side of the
--   'Either' input value.
splitEvents :: FRP (Event (Either a b)) (Event a, Event b)
splitEvents = (&&&) (mapMaybeEvents maybeLeft) (mapMaybeEvents maybeRight)
  where
    maybeLeft (Left  x) = Just x
    maybeLeft (Right _) = Nothing

    maybeRight (Left  _) = Nothing
    maybeRight (Right x) = Just x


-- | FRP equivalent to Maybe's 'catMaybes'.
filterJusts :: FRP (Event (Maybe a)) (Event a)
filterJusts = mapMaybeEvents id


-- | An FRP system that always keeps the last event value.
--
--   This is a variant of 'foldEvents' that always ignores the old behavior value.
holdLast :: a -> FRP (Event a) (Behavior a)
holdLast init = foldEvents (\_ n -> n) init
