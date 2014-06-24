module FRPZero where

import Prelude hiding ((.), id)

import Control.Category
import Control.Arrow

data State event state = State state (event -> State event state)

instance (Show state) => Show (State event state) where
  show (State s _) = show s

foldp :: state -> (event -> state -> state) -> State event state
foldp init f = State init sf
  where
    sf event = foldp (f event init) f

step event (State s sf) = sf event

before :: (e -> e') -> State e' s -> State e s
before f (State s sf) = State s (newsf sf)
  where
    newsf sf event = let (State s' sf') = sf (f event)
                     in State s' (newsf sf')

after :: (b -> b') -> State a b -> State a b'
after f (State s sf) = State (f s) (newsf sf)
  where
    newsf sf event = let (State s' sf') = sf event
                     in State (f s') (newsf sf')


merge :: (stateleft -> stateright -> state) -> State e stateleft -> State e stateright -> State e state
merge f (State sl sfl) (State sr sfr) = State (f sl sr) (newsf sfl sfr)
  where
    newsf sfl sfr event = let (State sl' sfl') = sfl event
                              (State sr' sfr') = sfr event
                          in State (f sl' sr') (newsf sfl' sfr')
