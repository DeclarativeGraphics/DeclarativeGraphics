module FRP.Base
  (FRP
  ,Event
  ,Behavior
  ,behaviorValue
  ,onBehavior
  ,mergeBehaviors
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
  ,foldEvents'
  ,accumEvents
  ,filterEvents
  ,mapMaybeEvents
  ,loopInit
  ,loopFold
  )

where



---- FRP BASE -----

-- | An FRP system is a function that takes as input some value of type a
--   and has as output some value of type b plus another FRP system from a to b
--   (the "new state" of the FRP system).
data FRP a b = Step (a -> (FRP a b, b))

-- | Execute one step in an FRP system.
step :: FRP a b -> a -> (FRP a b, b)
step (Step f) input = f input

-- | Turn any function f into a constant FRP system, i.e. an FRP system that never changes.
liftFRP :: (a -> b) -> FRP a b
liftFRP f = Step $ \input -> (liftFRP f, f input)


-- | An event is a either some value or no value (conceptionally equivalent to Maybe)
data Event a = Event a | NoEvent deriving (Show)

-- | Equivalent to Maybe's 'fmap'.
onEvent :: (a -> b) -> Event a -> Event b
onEvent f NoEvent   = NoEvent
onEvent f (Event x) = Event $ f x

-- | Equivalent to Maybe's 'maybe'.
withEvent :: b -> (a -> b) -> Event a -> b
withEvent d f NoEvent   = d
withEvent d f (Event x) = f x


-- | Equivalent to Maybe's 'fromMaybe'
fromEvent :: a -> Event a -> a
fromEvent d NoEvent   = d
fromEvent d (Event x) = x


-- | A behavior is a constant.
data Behavior a = Behavior a deriving (Show)

behaviorValue :: Behavior a -> a
behaviorValue (Behavior x) = x

-- | fmap
onBehavior :: (a -> b) -> Behavior a -> Behavior b
onBehavior f (Behavior x) = Behavior (f x)

mergeBehaviors :: (a -> b -> c) -> Behavior a -> Behavior b -> Behavior c
mergeBehaviors f (Behavior x) (Behavior y) = Behavior (f x y)



----- PRIMITIVES -----

-- | An FRP system that applies some function f to all input events.
mapEvents :: (a -> b) -> FRP (Event a) (Event b)
mapEvents = liftFRP . onEvent

-- | An FRP system that applies some function f to a behavior.
mapBehavior :: (a -> b) -> FRP (Behavior a) (Behavior b)
mapBehavior = liftFRP . onBehavior

-- | Merge two events, preferring the left event if both events have a value.
mergeEvents :: Event a -> Event a -> Event a
mergeEvents ev1 ev2 = case ev1 of
  Event _ -> ev1
  _       -> ev2

-- | A constant FRP system that always has the same behavior value.
constBehavior :: a -> FRP i (Behavior a)
constBehavior x = liftFRP $ const (Behavior x)


-- | Compose/chain two FRP systems. They must have compatible types, i.e.
--   the first system's output type must be the same as the second's system
--   input type.
--
--   This is the FRP equivalent to (reversed-order) function composition.
andThen :: FRP a b -> FRP b c -> FRP a c
andThen a b = Step $ \input -> let (newA, outA) = step a input
                                   (newB, outB) = step b outA
                               in (andThen newA newB, outB)

-- | Turn two FRP systems into a single FRP system that operates on a 2-tuple.
--
--   This is the definition for '(***)' of the 'FRP' Arrow instance.
parallel :: FRP ai ao -> FRP bi bo -> FRP (ai,bi) (ao,bo)
parallel a b = Step $ \(ai,bi) -> let (newA, outA) = step a ai
                                      (newB, outB) = step b bi
                                  in (parallel newA newB, (outA,outB))

-- | An FRP system that merges its input, a 2-tuple,
--   into a single value using the given function 'f'.
merge :: (a -> b -> c) -> FRP (a,b) c
merge f = liftFRP (uncurry f)

-- | Keep an event only when a predicate behavior is true (else turn it into 'NoEvent').
keepWhen :: Behavior Bool -> Event a -> Event a
keepWhen pred ev = if behaviorValue pred then ev else NoEvent

-- | Keep an event only when a predicate behavior is false (else turn it into 'NoEvent').
dropWhen :: Behavior Bool -> Event a -> Event a
dropWhen = keepWhen . onBehavior not

-- | Sample a behavior on an event: Take samples from the behavior whenever the
--   event has a value (is not 'NoEvent').
sampleOn :: Behavior a -> Event b -> Event a
sampleOn behavior = onEvent (\_ -> behaviorValue behavior)

-- | An FRP system that folds a stream of events into a behavior.
--
--   The resulting behavior keeps its value until the next occurence of an event,
--   at which point the new value is calculated using the old behavior value and
--   the new event value using the folding function 'f'.
--
--   This is conceptionally similar to 'foldl'.
foldEvents :: (b -> a -> b) -> b -> FRP (Event a) (Behavior b)
foldEvents f init = Step $ withEvent keepOldValue foldSingleEvent
  where
    keepOldValue = (foldEvents f init, Behavior init)
    foldSingleEvent x = let newState = f init x in (foldEvents f newState, Behavior newState)

-- TODO: terrible name
foldEvents' :: (a -> b) -> b -> FRP (Event a) (Behavior b)
foldEvents' f empty = liftFRP (\input -> withEvent (Behavior empty) (Behavior . f) input)

-- | Similar to 'foldEvents' but takes the folding function from the *events*.
accumEvents :: a -> FRP (Event (a -> a)) (Behavior a)
accumEvents init = Step $ withEvent keepOldValue accumSingleEvent
  where
    keepOldValue = (accumEvents init, Behavior init)
    accumSingleEvent f = let newState = f init in (accumEvents newState, Behavior newState)

-- | An FRP system that keeps only events that match a predicate.
filterEvents :: (a -> Bool) -> FRP (Event a) (Event a)
filterEvents pred = liftFRP filterPred
  where
    filterPred (Event x) | pred x = Event x
    filterPred _ = NoEvent

-- | FRP equivalent to Maybe's 'mapMaybe'.
mapMaybeEvents :: (a -> Maybe b) -> FRP (Event a) (Event b)
mapMaybeEvents f = liftFRP $ withEvent NoEvent (maybe NoEvent Event . f)


----- NOT-SO-PRIMITIVES -----

-- | Given a merge function and two FRP systems of the same input type,
--   make an FRP system that that "forks" the input, runs both FRP systems
--   independently, and then merges their outputs using the merge function.
--
--   The resulting FRP system could be drawn like a diamond:
--              .
--              .
--              .
--            input
--          /       \
--         /         \
--      left FRP  right FRP
--         \         /
--          \       /
--           +-----+
--           |  f  |
--           +--+--+
--              |
--              .
--              .
--              .
forkMerge :: (a -> b -> c) -> FRP i a -> FRP i b -> FRP i c
forkMerge f a b = liftFRP (\x -> (x, x))
                  `andThen` parallel a b
                  `andThen` merge f

-- | Feed a single event value into an FRP system, returning both the new FRP
--   system and the FRP's output value of that step.
stepEvent :: FRP (Event a) b -> a -> (FRP (Event a) b, b)
stepEvent frp x = step frp (Event x)


----- LOOPING -----

-- | Hide some FRP system's state.
loopInit :: state -> FRP (i,state) (o,state) -> FRP i o
loopInit init frp = Step $ \input -> let (newFrp,(outFrp,newState)) = step frp (input,init)
                                     in (loopInit newState newFrp, outFrp)

-- | Variant of 'loopInit' that expects the original FRP system's hidden state
--   to be events. These events are folded into a behavior (like 'foldEvents' does)
--   that is then supplied as input to the original FRP system.
loopFold :: state -> FRP (i, Behavior state) (o, Event state) -> FRP i o
loopFold init frp = Step $ \input -> let (newFrp, (out,newStateEvent)) = step frp (input, Behavior init)
                                     in (loopFold (fromEvent init newStateEvent) newFrp, out)
