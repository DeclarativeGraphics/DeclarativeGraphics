module TextInput where

import Utils

type TextInput = ([Char],[Char])

toTextInput str = (reverse str, [])

fromTextInput (prev, after) = reverse prev ++ after

textInputInsert c (prev, after) = (c : prev, after)

textInputDelete (prev, after) = case prev of
  (_ : restPrev) -> Just (restPrev, after)
  [] -> Nothing

textInputMoveLeft (left, right) = case left of
  (leftNeighbor : restLeft) -> Just (restLeft, leftNeighbor : right)
  [] -> Nothing

textInputMoveRight (left, right) = case right of
  (rightNeighbor : restRight) -> Just (rightNeighbor : left, restRight)
  [] -> Nothing

isTextInputEmpty (left, right) = null left && null right

emptyTextInput = toTextInput ""
