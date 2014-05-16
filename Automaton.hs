module Automaton where

import Utils

----- FRP BASE -----

data Automaton a b = Step (a -> (Automaton a b, b))

data Event a = Event a | NoEvent
data Behavior a = Behavior a

step :: Automaton a b -> a -> (Automaton a b, b)
step (Step f) input = f input

stepEvent :: Automaton (Event a) b -> a -> (Automaton (Event a) b, b)
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

mapEvent :: (a -> b) -> Automaton (Event a) (Event b)
mapEvent f = Step <| \ input -> (mapEvent f, onEvent f input)

mergeEvents :: Event a -> Event a -> Event a
mergeEvents ev1 ev2 = case ev1 of
  Event _ -> ev1
  _       -> ev2

mapBehavior :: (a -> b) -> Automaton (Behavior a) (Behavior b)
mapBehavior f = Step <| \ input -> (mapBehavior f, onBehavior f input)

constBehavior :: a -> Automaton i (Behavior a)
constBehavior x = Step <| \ _ -> (constBehavior x, Behavior x)



andThen :: Automaton a b -> Automaton b c -> Automaton a c
andThen a b = Step (\ input -> let (newA, outA) = step a input
                                   (newB, outB) = step b outA
                               in (andThen newA newB, outB))

(>>>) :: Automaton a b -> Automaton b c -> Automaton a c
(>>>) = andThen
(<<<) :: Automaton b c -> Automaton a b -> Automaton a c
(<<<) = flip (>>>)

swap :: Automaton (a,b) (b,a)
swap = liftA (\ (x,y) -> (y,x))

                                
keepWhen :: Behavior Bool -> Event a -> Event a
keepWhen pred ev = if behaviorValue pred then ev else NoEvent

dropWhen :: Behavior Bool -> Event a -> Event a
dropWhen = keepWhen . onBehavior not

identity :: Automaton a a
identity = liftA id

foldEvents :: (b -> a -> b) -> b -> Automaton (Event a) (Behavior b)
foldEvents f init = Step <| \ input -> case input of
  NoEvent -> (foldEvents f init, Behavior init)
  Event ev -> let newState = f init ev
              in (foldEvents f newState, Behavior newState)

accumEvents :: a -> Automaton (Event (a -> a)) (Behavior a)
accumEvents init = Step <| \ input -> case input of
  NoEvent -> (accumEvents init, Behavior init)
  Event f -> let newState = f init
             in (accumEvents newState, Behavior newState)

splitEvents :: Automaton (Event (Either a b)) (Event a, Event b)
splitEvents = Step <| \ input -> (splitEvents,
                                  case input of
                                    NoEvent -> (NoEvent, NoEvent)
                                    Event (Left l) -> (Event l, NoEvent)
                                    Event (Right r) -> (NoEvent, Event r))


filterEvents :: (a -> Bool) -> Automaton (Event a) (Event a)
filterEvents pred = Step (\ input -> (filterEvents pred,
                                      case input of
                                        Event x -> if pred x then Event x else NoEvent
                                        NoEvent -> NoEvent))


filterJust :: Automaton (Event (Maybe a)) (Event a)
filterJust = Step (\ input -> case input of
                                NoEvent -> (filterJust, NoEvent)
                                Event maybeX -> (filterJust, maybe NoEvent Event maybeX))

holdLast :: a -> Automaton (Event a) (Behavior a)
holdLast init = foldEvents (\_ n -> n) init

splitMerge :: (a -> b -> c) -> Automaton g a -> Automaton g b -> Automaton g c
splitMerge f a b = Step (\ input -> let (newA, outA) = step a input
                                        (newB, outB) = step b input
                                    in (splitMerge f newA newB, f outA outB))

parallel :: Automaton ai ao -> Automaton bi bo -> Automaton (ai,bi) (ao,bo)
parallel a b = fork (a <<< liftA fst) (b <<< liftA snd)

fork :: Automaton g a -> Automaton g b -> Automaton g (a,b)
fork a b = splitMerge (,) a b

merge :: (a -> b -> c) -> Automaton (a,b) c
merge f = liftA (uncurry f)

liftA :: (a -> b) -> Automaton a b
liftA f = Step (\ input -> (liftA f, f input)) 


loop :: state -> Automaton (i,state) (o,state) -> Automaton i o
loop init aut = Step (\ input -> let (newAut,(outAut,stateAut)) = step aut (input,init)
                                 in (loop stateAut newAut, outAut))
