module Graphics.Prototype.TextEdit where

data TextEdit = TextEdit String String

emptyText = TextEdit "" ""

showTextEdit (TextEdit before after) = reverse before ++ "I" ++ after

teBackspace (TextEdit before after) = TextEdit (safeTail before) after
teDelete    (TextEdit before after) = TextEdit before (safeTail after)

tePut c     (TextEdit before after) = TextEdit (c : before) after

teHome (TextEdit before after) = TextEdit "" (reverse before ++ after)
teEnd  (TextEdit before after) = TextEdit (before ++ reverse after) ""

teLeft  (TextEdit (c:before) after) = TextEdit before (c : after)
teLeft  te                          = te

teRight (TextEdit before (c:after)) = TextEdit (c : before) after
teRight te                          = te

safeTail (x:xs) = xs
safeTail []     = []
