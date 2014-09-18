module Focus where

type Focus a = ([a],a,[a])

focus (focus:more) = ([],focus,more)
getFocused (_,focus,_) = focus
setFocused x = onFocused (const x)
focusAsList (left, focus, right) = reverse left ++ [focus] ++ right

moveRight (left, focus, x:right) = Just (focus:left, x, right)
moveRight (left, focus, [])      = Nothing

moveLeft (x:left, focus, right) = Just (left, x, focus:right)
moveLeft ([], focus, right)     = Nothing

moveElementRight (left, focus, x:right) = Just (x:left, focus, right)
moveElementRight (left, focus, [])      = Nothing

moveElementLeft  (x:left, focus, right) = Just (left, focus, x:right)
moveElementLeft  ([], focus, right)     = Nothing

onFocused f (left, focus, right) = (left, f focus, right)
onFocusedLifted f (left, focus, right) = do newFocus <- f focus
                                            return (left, newFocus, right)
mapFocus f (left, focus, right) = (map f left, f focus, map f right)

mapFocusParts leftF focusedF rightF (left, focus, right)
  = (map leftF left, focusedF focus, map rightF right)

insertFocus x (left, focus, right) = Just (focus:left, x, right)

deleteFocus (left, focus, r:right) = Just (left, r, right)
deleteFocus (l:left, focus, right) = Just (left, l, right)
deleteFocus _                      = Nothing

