module FRPZero where

data State event state = State (event -> State event state) state

state (State _ s) = s

instance (Show state) => Show (State event state) where
  show (State _ s) = show s

foldp :: (event -> state -> state) -> state -> State event state
foldp f init = State sf init
  where
    sf event = foldp f (f event init)

accum :: state -> State (state -> state) state
accum state = State sf state
  where
    sf changeevent = accum (changeevent state)

step :: event -> State event state -> State event state
step event (State sf s) = sf event

before :: (event' -> event) -> State event state -> State event' state
before f (State sf s) = State newsf s
  where
    newsf event = before f (sf (f event))

f >>^ state = f `before` state

after :: (state -> state') -> State event state -> State event state'
after f (State sf s) = State newsf (f s)
  where
    newsf event = after f (sf event)

state ^>> f = f `after` state


maybeFilter :: State event state -> State (Maybe event) state
maybeFilter (State sf s) = State sf' s
  where
    sf' (Just event) = maybeFilter (sf event)
    sf' Nothing      = maybeFilter (State sf s)


merge :: (stateleft -> stateright -> state) -> State e stateleft -> State e stateright -> State e state
merge f (State sfl sl) (State sfr sr) = State newsf (f sl sr)
  where
    newsf event = merge f (sfl event) (sfr event)

loop :: State (event,hiddenstate) (state,hiddenstate) -> State event state
loop (State sf (state, hiddenstate)) = State newsf state
  where
    newsf event = loop (sf (event,hiddenstate))

constant :: state -> State event state
constant state = State (const (constant state)) state
