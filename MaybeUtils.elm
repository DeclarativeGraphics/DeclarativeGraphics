module MaybeUtils where

import Maybe (..)

maybeVal `orElse` def = maybe def id maybeVal

maybeDo f maybeVal = maybe Nothing (Just . f) maybeVal

foldMaybe f x = f x `orElse` x


catMaybes ls = case ls of
  [] -> []
  (Just x :: xs) -> x :: catMaybes xs
  (Nothing :: xs) -> catMaybes xs
