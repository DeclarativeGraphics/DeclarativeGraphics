module Matching where

import Control.Applicative (Applicative(..))
import Data.Monoid (Monoid(..))


newtype Matcher input err a = M (input -> Result err a)
data Result e s = Error e | Success s


instance Functor (Matcher input err) where
  fmap f (M matcher) = M (fmap f . matcher)

instance Monoid err => Applicative (Matcher input err) where
  pure x = M $ \input -> pure x
  (M f) <*> (M arg) = M (\input -> f input <*> arg input)



instance Functor (Result e) where
  fmap f (Success success) = Success (f success)
  fmap f (Error error)     = Error error

instance Monoid e => Applicative (Result e) where
  pure = Success

  (Success f)     <*> (Success arg)   = Success (f arg)
  (Error errors1) <*> (Error errors2) = Error (errors1 `mappend` errors2)
  (Error errors)  <*> nonerror        = Error errors
  nonerror        <*> (Error errors)  = Error errors



runMatcher (M matcher) = matcher
match matcher = getResult . runMatcher matcher

getResult (Error err)    = Left err
getResult (Success succ) = Right succ
