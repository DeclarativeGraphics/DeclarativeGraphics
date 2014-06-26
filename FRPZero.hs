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
before f (State sf s) = State (newsf f sf) s
  where
    newsf :: (event' -> event) -> (event -> State event state) -> event' -> State event' state
    newsf f sf event = let (State sf' s') = sf (f event)
                       in State (newsf f sf') s'

f >>^ state = f `before` state

after :: (state -> state') -> State event state -> State event state'
after f (State sf s) = State (newsf f sf) (f s)
  where
    newsf :: (state -> state') -> (event -> State event state) -> event -> State event state'
    newsf f sf event = let (State sf' s') = sf event
                       in State (newsf f sf') (f s')

state ^>> f = f `after` state


merge :: (stateleft -> stateright -> state) -> State e stateleft -> State e stateright -> State e state
merge f (State sfl sl) (State sfr sr) = State (newsf f sfl sfr) (f sl sr)
  where
    newsf :: (l -> r -> o)
          -> (event -> State event l)
          -> (event -> State event r)
          -> event
          -> State event o
    newsf f sfl sfr event = let (State sfl' sl') = sfl event
                                (State sfr' sr') = sfr event
                            in State (newsf f sfl' sfr') (f sl' sr')

loop :: State (event,hiddenstate) (state,hiddenstate) -> State event state
loop (State sf (s, hs)) = State (newsf hs sf) s
  where
    newsf hs sf event = let (State sf' (s', hs')) = sf (event,hs)
                        in State (newsf hs' sf') s'

constant :: state -> State event state
constant state = State (const (constant state)) state
