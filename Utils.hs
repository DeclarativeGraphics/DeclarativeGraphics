module Utils where

import Data.Maybe

(<|) = ($)
(|>) = flip ($)

maybeApply :: (a -> Maybe a) -> a -> a
maybeApply f x = fromMaybe x (f x)
