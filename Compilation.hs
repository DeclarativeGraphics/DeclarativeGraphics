module Compilation where

import Control.Applicative (Applicative(..),liftA,liftA2)


------------- GENERAL COMPILATION ----------------


data Compilation a b = CompileError [ErrorContext a] | CompileResult b
data ErrorContext a = ErrorMessage String
                    | ErrorSource String a
                    | ErrorContext String [ErrorContext a]
                    deriving Show

instance Functor (Compilation a) where
  fmap f (CompileResult result) = CompileResult (f result)
  fmap f (CompileError error)   = CompileError error

instance Applicative (Compilation a) where
  pure = CompileResult

  (CompileResult f)      <*> (CompileResult arg)    = CompileResult (f arg)
  (CompileError errors1) <*> (CompileError errors2) = CompileError (errors1 ++ errors2)
  (CompileError errors)  <*> nonerror               = CompileError errors
  nonerror               <*> (CompileError errors)  = CompileError errors


getCompilation (CompileError err) = Left err
getCompilation (CompileResult res) = Right res


compileError :: String -> a -> Compilation a b
compileError what source = CompileError [ErrorSource what source]

simpleError :: String -> Compilation a b
simpleError message = CompileError [ErrorMessage message]

errs1 `compileErrorPlus` errs2 = errs1 ++ errs2

(left `orTry` right) expr = case left expr of
  CompileError lefterr    -> case right expr of
    CompileError righterr -> CompileError $ lefterr ++ righterr
    rightsuccess          -> rightsuccess
  leftsuccess             -> leftsuccess

getBoth :: Applicative f => f a -> f b -> f (a, b)
getBoth = liftA2 (,)

lmany melem [] = pure []
lmany melem (x:xs) = liftA2 (:) (melem x) (lmany melem xs)

lmany1 msub = liftA (uncurry (:)) . (msub `lcons` lmany msub)

llist []     []     = pure []
llist (m:ms) (x:xs) = liftA2 (:) (m x) (llist ms xs)
llist []     _      = simpleError "too many elements"
llist _      []     = simpleError "too few elements"

lcons mhead mtail (x:xs) = liftA2 (,) (mhead x) (mtail xs)
lcons _     _     []     = simpleError "expected non-empty list"

withContext context (CompileResult result) = CompileResult result
withContext context (CompileError error)   = CompileError [ErrorContext context error]
