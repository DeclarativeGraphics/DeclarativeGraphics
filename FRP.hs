module FRP where

import Prelude hiding ((.),id)
import Control.Category
import Control.Arrow
import Utils

instance Category FRP where
  id = identity
  (.) = flip andThen

instance Arrow FRP where
  arr = liftFRP
  first frp = parallel frp identity
  second frp = parallel identity frp
  (***) = parallel
  (&&&) = fork

instance ArrowLoop FRP where
  loop aut = Step (\ input -> let (newAut,(outAut,state)) = step aut (input,state)
                              in (loop newAut, outAut))


----- FRP BASE -----

data FRP a b = Step (a -> (FRP a b, b))

data Event a = Event a | NoEvent deriving (Show)
data Behavior a = Behavior a deriving (Show)

step :: FRP a b -> a -> (FRP a b, b)
step (Step f) input = f input

stepEvent :: FRP (Event a) b -> a -> (FRP (Event a) b, b)
stepEvent automaton x = step automaton (Event x)


----- HELPERS -----

onEvent :: (a -> b) -> Event a -> Event b
onEvent f ev = case ev of
  Event x -> Event (f x)
  NoEvent -> NoEvent

behaviorValue :: Behavior a -> a
behaviorValue (Behavior x) = x

onBehavior :: (a -> b) -> Behavior a -> Behavior b
onBehavior f (Behavior x) = Behavior (f x)

onBehavior2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
onBehavior2 f (Behavior x) (Behavior y) = Behavior (f x y)


----- COMBINATORS ETC. -----

liftFRP :: (a -> b) -> FRP a b
liftFRP f = Step (\ input -> (liftFRP f, f input)) 

mapEvents :: (a -> b) -> FRP (Event a) (Event b)
mapEvents f = Step <| \ input -> (mapEvents f, onEvent f input)

mergeEvents :: Event a -> Event a -> Event a
mergeEvents ev1 ev2 = case ev1 of
  Event _ -> ev1
  _       -> ev2

mapBehavior :: (a -> b) -> FRP (Behavior a) (Behavior b)
mapBehavior f = Step <| \ input -> (mapBehavior f, onBehavior f input)

constBehavior :: a -> FRP i (Behavior a)
constBehavior x = Step <| \ _ -> (constBehavior x, Behavior x)



andThen :: FRP a b -> FRP b c -> FRP a c
andThen a b = Step (\ input -> let (newA, outA) = step a input
                                   (newB, outB) = step b outA
                               in (andThen newA newB, outB))

swap :: FRP (a,b) (b,a)
swap = liftFRP (\ (x,y) -> (y,x))

                                
keepWhen :: Behavior Bool -> Event a -> Event a
keepWhen pred ev = if behaviorValue pred then ev else NoEvent

dropWhen :: Behavior Bool -> Event a -> Event a
dropWhen = keepWhen . onBehavior not

identity :: FRP a a
identity = liftFRP id

foldEvents :: (b -> a -> b) -> b -> FRP (Event a) (Behavior b)
foldEvents f init = Step <| \ input -> case input of
  NoEvent -> (foldEvents f init, Behavior init)
  Event ev -> let newState = f init ev
              in (foldEvents f newState, Behavior newState)

accumEvents :: a -> FRP (Event (a -> a)) (Behavior a)
accumEvents init = Step <| \ input -> case input of
  NoEvent -> (accumEvents init, Behavior init)
  Event f -> let newState = f init
             in (accumEvents newState, Behavior newState)

splitEvents :: FRP (Event (Either a b)) (Event a, Event b)
splitEvents = Step <| \ input -> (splitEvents,
                                  case input of
                                    NoEvent -> (NoEvent, NoEvent)
                                    Event (Left l) -> (Event l, NoEvent)
                                    Event (Right r) -> (NoEvent, Event r))


filterEvents :: (a -> Bool) -> FRP (Event a) (Event a)
filterEvents pred = Step (\ input -> (filterEvents pred,
                                      case input of
                                        Event x -> if pred x then Event x else NoEvent
                                        NoEvent -> NoEvent))


filterJust :: FRP (Event (Maybe a)) (Event a)
filterJust = Step (\ input -> case input of
                                NoEvent -> (filterJust, NoEvent)
                                Event maybeX -> (filterJust, maybe NoEvent Event maybeX))

holdLast :: a -> FRP (Event a) (Behavior a)
holdLast init = foldEvents (\_ n -> n) init

splitMerge :: (a -> b -> c) -> FRP g a -> FRP g b -> FRP g c
splitMerge f a b = Step (\ input -> let (newA, outA) = step a input
                                        (newB, outB) = step b input
                                    in (splitMerge f newA newB, f outA outB))

delay :: FRP (Event a) (Event a)
delay = delay' NoEvent

delay' :: Event a -> FRP (Event a) (Event a)
delay' lastEvent = Step <| \ input -> (delay' input, lastEvent)

parallel :: FRP ai ao -> FRP bi bo -> FRP (ai,bi) (ao,bo)
parallel a b = fork (a <<< liftFRP fst) (b <<< liftFRP snd)

fork :: FRP g a -> FRP g b -> FRP g (a,b)
fork a b = splitMerge (,) a b

merge :: (a -> b -> c) -> FRP (a,b) c
merge f = liftFRP (uncurry f)


--- LOOPING ---

loopInit :: state -> FRP (i,state) (o,state) -> FRP i o
loopInit init frp = Step <| \ input -> let (newFrp,(outFrp,newState)) = step frp (input,init)
                                       in (loopInit newState newFrp, outFrp)

loopFold :: state -> FRP (i, Behavior state) (o, Event state) -> FRP i o
loopFold init frp = Step <| \ input -> let (newFrp, (out,newStateEvent)) = step frp (input, Behavior init)
                                           newState = case newStateEvent of
                                                        NoEvent -> init
                                                        Event x -> x
                                       in (loopFold newState newFrp, out)
