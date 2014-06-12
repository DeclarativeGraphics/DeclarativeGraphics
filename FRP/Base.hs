module FRP.Base
  (FRP
  ,Event
  ,Behavior
  ,behaviorValue
  ,onBehavior
  ,onBehavior2
  ,stepEvent
  ,liftFRP
  ,mapEvents
  ,mapBehavior
  ,mergeEvents
  ,constBehavior
  ,andThen
  ,parallel
  ,merge
  ,forkMerge
  ,keepWhen
  ,dropWhen
  ,sampleOn
  ,foldEvents
  ,accumEvents
  ,filterEvents
  ,mapMaybeEvents
  ,loopInit
  ,loopFold
  )

where



---- FRP BASE -----

data FRP a b = Step (a -> (FRP a b, b))

step :: FRP a b -> a -> (FRP a b, b)
step (Step f) input = f input

liftFRP :: (a -> b) -> FRP a b
liftFRP f = Step $ \input -> (liftFRP f, f input)


data Event a = Event a | NoEvent deriving (Show)

withEvent :: (a -> b) -> b -> Event a -> b
withEvent f d NoEvent   = d
withEvent f d (Event x) = f x

eventValueOr :: Event a -> a -> a
eventValueOr NoEvent   d = d
eventValueOr (Event x) _ = x

onEvent :: (a -> b) -> Event a -> Event b
onEvent f NoEvent   = NoEvent
onEvent f (Event x) = Event $ f x


data Behavior a = Behavior a deriving (Show)

behaviorValue :: Behavior a -> a
behaviorValue (Behavior x) = x

onBehavior :: (a -> b) -> Behavior a -> Behavior b
onBehavior f (Behavior x) = Behavior (f x)

onBehavior2 :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
onBehavior2 f (Behavior x) (Behavior y) = Behavior (f x y)



----- PRIMITIVES -----

mapEvents :: (a -> b) -> FRP (Event a) (Event b)
mapEvents = liftFRP . onEvent

mapBehavior :: (a -> b) -> FRP (Behavior a) (Behavior b)
mapBehavior = liftFRP . onBehavior

mergeEvents :: Event a -> Event a -> Event a
mergeEvents ev1 ev2 = case ev1 of
  Event _ -> ev1
  _       -> ev2

constBehavior :: a -> FRP i (Behavior a)
constBehavior x = liftFRP $ const (Behavior x)


andThen :: FRP a b -> FRP b c -> FRP a c
andThen a b = Step $ \input -> let (newA, outA) = step a input
                                   (newB, outB) = step b outA
                               in (andThen newA newB, outB)

parallel :: FRP ai ao -> FRP bi bo -> FRP (ai,bi) (ao,bo)
parallel a b = Step $ \(ai,bi) -> let (newA, outA) = step a ai
                                      (newB, outB) = step b bi
                                  in (parallel newA newB, (outA,outB))

merge :: (a -> b -> c) -> FRP (a,b) c
merge f = liftFRP (uncurry f)

keepWhen :: Behavior Bool -> Event a -> Event a
keepWhen pred ev = if behaviorValue pred then ev else NoEvent

dropWhen :: Behavior Bool -> Event a -> Event a
dropWhen = keepWhen . onBehavior not

sampleOn :: Behavior a -> Event b -> Event a
sampleOn behavior = onEvent (\_ -> behaviorValue behavior)

foldEvents :: (b -> a -> b) -> b -> FRP (Event a) (Behavior b)
foldEvents f init = Step $ withEvent (\ev -> let newState = f init ev
                                                 in (foldEvents f newState, Behavior newState))
                           (foldEvents f init, Behavior init)

accumEvents :: a -> FRP (Event (a -> a)) (Behavior a)
accumEvents init = Step $ withEvent (\f -> let newState = f init
                                               in (accumEvents newState, Behavior newState))
                          (accumEvents init, Behavior init)

filterEvents :: (a -> Bool) -> FRP (Event a) (Event a)
filterEvents pred = Step $ \input -> (filterEvents pred,
                                      case input of
                                        NoEvent -> NoEvent
                                        Event x -> if pred x then Event x
                                                   else NoEvent)

mapMaybeEvents :: (a -> Maybe b) -> FRP (Event a) (Event b)
mapMaybeEvents f = Step $ withEvent
  (\a -> (maybe (mapMaybeEvents f, NoEvent)
                (\b -> (mapMaybeEvents f, Event b))
                (f a)))
  (mapMaybeEvents f, NoEvent)



----- NOT-SO-PRIMITIVES -----

forkMerge :: (a -> b -> c) -> FRP i a -> FRP i b -> FRP i c
forkMerge f a b = liftFRP (\x -> (x, x))
                  `andThen` parallel a b
                  `andThen` merge f

stepEvent :: FRP (Event a) b -> a -> (FRP (Event a) b, b)
stepEvent frp x = step frp (Event x)


----- LOOPING -----

loopInit :: state -> FRP (i,state) (o,state) -> FRP i o
loopInit init frp = Step $ \input -> let (newFrp,(outFrp,newState)) = step frp (input,init)
                                     in (loopInit newState newFrp, outFrp)

loopFold :: state -> FRP (i, Behavior state) (o, Event state) -> FRP i o
loopFold init frp = Step $ \input -> let (newFrp, (out,newStateEvent)) = step frp (input, Behavior init)
                                     in (loopFold (newStateEvent `eventValueOr` init) newFrp, out)
