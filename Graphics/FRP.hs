{-# LANGUAGE GADTs #-}
module Graphics.FRP where

import Control.Applicative
import Control.Monad

data Event a = Event a | NoEvent deriving Show

newtype Behavior a = Behavior { behaviorValue :: a } deriving Show

data FRP a b where
  Constant :: b -> FRP a b
  Pure     :: (a -> b) -> FRP a b
  Stateful :: (a -> b -> b) -> b -> FRP a b
  Connect  :: FRP a i -> FRP i b -> FRP a b
  Merge    :: (i -> j -> b) -> FRP a i -> FRP a j -> FRP a b
  OnEvent  :: FRP a b -> FRP (Event a) b

instance Show (FRP a b) where
  show (Constant _) = "const"
  show (Pure _)     = "pure"
  show (Stateful _ _) = "stateful"
  show (Connect a b)  = show a ++ " -> " ++ show b
  show (Merge _ a b)  = "(" ++ show a ++ " | " ++ show b ++ ")"
  show (OnEvent f)    = "(onEvent " ++ show f ++ ")"

run :: FRP a b -> a -> (b, FRP a b)
run c@(Constant k)      _ = (k, c)
run p@(Pure f)          x = (f x, p)
run (Stateful f value)  x = (newvalue, Stateful f newvalue)
  where newvalue = f x value
run (Connect frp0 frp1) x = (res1, Connect nfrp0 nfrp1)
  where
    (res0, nfrp0) = run frp0 x
    (res1, nfrp1) = run frp1 res0
run (Merge f frp0 frp1) x = (merged, Merge f nfrp0 nfrp1)
  where
    (res0, nfrp0) = run frp0 x
    (res1, nfrp1) = run frp1 x
    merged = f res0 res1
run (OnEvent frp)       x = case x of
                              Event e -> let (res, nfrp) = run frp e
                                         in (res, OnEvent nfrp)
                              NoEvent -> (getState frp, OnEvent frp)

getState :: FRP a b -> b
getState (Constant c)        = c
getState (Pure f)            = f $ error "pure has no state"
getState (Stateful _ state)  = state
getState (Connect frp (Pure f)) = f (getState frp)
getState (Connect _ frp)     = getState frp
getState (Merge f frp0 frp1) = f (getState frp0) (getState frp1)
getState (OnEvent frp)       = getState frp


-------- BEHAVIOR -------

constant :: a -> FRP x (Behavior a)
constant = Constant . Behavior

mapBehavior :: (a -> b) -> FRP (Behavior a) (Behavior b)
mapBehavior f = Pure (Behavior . f . behaviorValue)


-------- EVENTS -------

mapEvent :: (a -> b) -> FRP (Event a) (Event b)
mapEvent f = OnEvent $ Pure (Event . f)


filterEvent :: (a -> Bool) -> FRP (Event a) (Event a)
filterEvent pred = OnEvent $ Pure filterEvent
 where
   filterEvent ev = if pred ev then Event ev else NoEvent
 
eventExtract :: (a -> Maybe b) -> FRP (Event a) (Event b)
eventExtract extract = OnEvent $ Pure (maybeToEvent . extract)

eventMergeLeft :: FRP g (Event a) -> FRP g (Event a) -> FRP g (Event a)
eventMergeLeft = Merge evMerge
  where
    evMerge x y = case x of
      Event _ -> x
      NoEvent -> y

eventMergeRight :: FRP g (Event a) -> FRP g (Event a) -> FRP g (Event a)
eventMergeRight = Merge evMerge
  where
    evMerge x y = case y of
      Event _ -> y
      NoEvent -> x

eventMergeWith :: (Maybe a -> Maybe a -> Maybe a) -> FRP g (Event a) -> FRP g (Event a) -> FRP g (Event a)
eventMergeWith f evfrp1 evfrp2 = Merge evMerge evfrp1 evfrp2
  where
    evMerge ev1 ev2 = maybeToEvent $ f (eventToMaybe ev1) (eventToMaybe ev2)

maybeToEvent (Just x) = Event x
maybeToEvent Nothing  = NoEvent

eventToMaybe (Event x) = Just x
eventToMaybe NoEvent   = Nothing



--------- CONVERT ----------

foldp :: (a -> v -> v) -> v -> FRP (Event a) (Behavior v)
foldp f init = OnEvent $ Stateful fOnBehavior (Behavior init)
 where fOnBehavior event (Behavior value) = Behavior (f event value)

sampleOn :: FRP a (Behavior v) -> FRP a (Event i) -> FRP a (Event v)
behavior `sampleOn` event = Merge sample event behavior
 where
   sample (Event _) (Behavior x) = Event x
   sample NoEvent   _            = NoEvent


--------- COMBINATORS --------

filterWhen :: FRP a (Event b) -> FRP a (Behavior Bool) -> FRP a (Event b)
event `filterWhen` behavior = Merge whenTrue behavior event
 where
   whenTrue (Behavior True)  x = x
   whenTrue (Behavior False) _ = NoEvent



connect :: FRP a i -> FRP i b -> FRP a b
connect (Connect a b)        c = connect a (connect b c)
connect (Pure f) (Pure g)      = Pure (g . f)
connect (Pure f) (Connect (Pure g) c) = Connect (Pure (g . f)) c
connect (OnEvent f) g = OnEvent (connect f g)
connect a b = Connect a b


merge :: (i -> j -> b) -> FRP a (Behavior i) -> FRP a (Behavior j) -> FRP a (Behavior b)
merge mrg (Pure f)    (Pure g)    = Pure $ \x -> Behavior $ mrg (behaviorValue $ f x) (behaviorValue $ g x)
merge mrg (OnEvent f) (OnEvent g) = OnEvent $ merge mrg f g
merge mrg a           b           = nativeMerge mrg a b

nativeMerge :: (i -> j -> b) -> FRP a (Behavior i) -> FRP a (Behavior j) -> FRP a (Behavior b)
nativeMerge mrg a b = Merge mrgBehaviors a b
  where mrgBehaviors (Behavior i) (Behavior j) = Behavior (mrg i j)



--------- INFIX STUFF ---------

infixr 1 >>>
(>>>) = connect
infixl 1 <<<
(<<<) = flip (>>>)

infixl 1 <~
(<~) :: (a -> b) -> FRP i (Behavior a) -> FRP i (Behavior b)
f <~ x = mapBehavior f <<< x
infixl 1 ~~
(~~) :: FRP i (Behavior (a -> b)) -> FRP i (Behavior a) -> FRP i (Behavior b)
f ~~ x = merge ($) f x
