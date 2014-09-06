module Utils where

import Data.Maybe

infixr 1 <|
(<|) = ($)
(|>) = flip ($)

infixl 1 #
(#) = ($)

maybeApply :: (a -> Maybe a) -> a -> a
maybeApply f x = fromMaybe x (f x)

Just x  `orElse` def = x
Nothing `orElse` def = def
