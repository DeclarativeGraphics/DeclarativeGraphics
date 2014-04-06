{-# LANGUAGE GADTs #-}
module Graphics.FRP where

import Control.Applicative
import Control.Monad

data Event a = Event a | NoEvent deriving Show

instance Monad Event where
  return = Event
  (Event e) >>= f = f e
  NoEvent   >>= f = NoEvent

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
run (Stateful f state)  x = (newstate, Stateful f newstate)
  where newstate = f x state
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

runList network xs = tail $ map fst $ scanl (run . snd) (undefined, network) xs

constant = Constant
mapB = Pure

input = mapB id

foldp :: (a -> s -> s) -> s -> FRP (Event a) s
foldp f state = OnEvent $ Stateful f state

sampleOn :: FRP a (Event i) -> FRP a v -> FRP a (Event v)
sampleOn = merge sample
 where
   sample (Event _) x = Event x
   sample NoEvent   _ = NoEvent

maskBy :: FRP a Bool -> FRP a (Event b) -> FRP a (Event b)
maskBy = merge maskEvent
 where
   maskEvent True  _ = NoEvent
   maskEvent False x = x

eventMap :: (a -> b) -> FRP (Event a) (Event b)
eventMap f = OnEvent $ mapB (Event . f)

eventsWhen :: FRP g Bool -> FRP g (Event a) -> FRP g (Event a)
eventsWhen = merge filterEvent
 where
   filterEvent True ev = ev
   filterEvent False _ = NoEvent

eventFilter :: (a -> Bool) -> FRP (Event a) (Event a)
eventFilter pred = OnEvent $ mapB filterEvent
 where
   filterEvent ev = if pred ev then Event ev else NoEvent
 
eventExtract :: (a -> Maybe b) -> FRP (Event a) (Event b)
eventExtract extract = OnEvent $ mapB extractEvent
 where
   extractEvent ev = case extract ev of
     Just res -> Event res
     Nothing  -> NoEvent

eventMerge :: FRP g (Event a) -> FRP g (Event a) -> FRP g (Event a)
eventMerge = merge evMerge
  where
    evMerge x y = case x of
      Event _ -> x
      NoEvent -> y

hold = foldp const

connect :: FRP a i -> FRP i b -> FRP a b
connect (Connect a b)        c = connect a (connect b c)
connect (Pure f) (Pure g)      = Pure (g . f)
connect (Pure f) (Connect (Pure g) c) = connect (Pure (g . f)) c
connect (OnEvent f) g = OnEvent (connect f g)
connect a b = Connect a b

merge :: (i -> j -> b) -> FRP a i -> FRP a j -> FRP a b
merge mrg (Pure f) (Pure g) = Pure $ \x -> mrg (f x) (g x)
merge mrg (OnEvent f) (OnEvent g) = OnEvent $ merge mrg f g
merge mrg a        b        = Merge mrg a b

infixr 1 >>>
(>>>) = connect
infixl 1 <<<
(<<<) = flip (>>>)

infixl 1 <~
(<~) :: (a -> b) -> FRP i a -> FRP i b
f <~ x = mapB f <<< x
infixl 1 ~~
(~~) :: FRP i (a -> b) -> FRP i a -> FRP i b
f ~~ x = merge ($) f x
