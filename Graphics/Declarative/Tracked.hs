module Graphics.Declarative.Tracked where

import Graphics.Declarative.Physical2D
import Graphics.Declarative.Bordered

import qualified Data.Vec2 as Vec2
import Data.Vec2 (Vec2)

import qualified Data.Map as Map
import Data.Map (Map)

data Tracked d a = Tracked d a

class TrackData d where
  combineData :: d -> d -> d
  modifyData :: (Vec2 -> Vec2) -> d -> d
  emptyData :: d

instance (TrackData d, Physical2D a) => Physical2D (Tracked d a) where
  move offset (Tracked dat a)   = Tracked (modifyData (Vec2.add offset) dat) (move offset a)
  rotate angle (Tracked dat a)  = Tracked (modifyData (Vec2.rotateBy angle) dat) (rotate angle a)
  scale factors (Tracked dat a) = Tracked (modifyData (Vec2.mul factors) dat) (scale factors a)
  atop (Tracked dat1 a1) (Tracked dat2 a2) = Tracked (combineData dat1 dat2) (atop a1 a2)
  empty = Tracked emptyData empty

type KeyTracked k a = Tracked (VecMap k) a

newtype VecMap k = VecMap { vecmap :: Map k Vec2 }

instance Ord k => TrackData (VecMap k) where
  combineData (VecMap inner1) (VecMap inner2) = VecMap $ Map.union inner1 inner2
  modifyData f (VecMap inner) = VecMap $ fmap f inner
  emptyData = VecMap Map.empty

addTracking :: (Physical2D a, Ord k) => k -> Vec2 -> Bordered a -> Bordered (KeyTracked k a)
addTracking key pos (Bordered border form) = Bordered border $ Tracked (VecMap $ Map.singleton key pos) form

addOriginTracking :: (Physical2D a, Ord k) => k -> Bordered a -> Bordered (KeyTracked k a)
addOriginTracking key form = addTracking key Vec2.zero form

getInner :: Tracked d a -> a
getInner (Tracked d a) = a

getTrackedPos :: Ord k => k -> Bordered (KeyTracked k a) -> Maybe Vec2
getTrackedPos key (Bordered _ (Tracked (VecMap vmap) _)) = Map.lookup key vmap

removeTracking :: Bordered (Tracked d a) -> Bordered a
removeTracking (Bordered border (Tracked _ a)) = Bordered border a
