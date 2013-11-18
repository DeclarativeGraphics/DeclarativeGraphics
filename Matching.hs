module Matching where

import Control.Applicative (Applicative(..))
import Data.Monoid (Monoid(..))


newtype Matcher i e a = M (i -> Result e a)
data Result e s = Error e | Success s


instance Functor (Matcher i e) where
  fmap f (M matcher) = M (fmap f . matcher)

instance Monoid e => Applicative (Matcher i e) where
  pure = M . const . pure
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




transform1 matcher submatcher = M . matcher $ runMatch submatcher
transform2 matcher subm0 subm1 = M $ matcher (runMatch subm0) (runMatch subm1)
transform3 matcher subm0 subm1 subm2
  = M $ matcher (runMatch subm0) (runMatch subm1) (runMatch subm2)



runMatch (M matcher) = matcher
match matcher = getResult . runMatch matcher

getResult (Error err)    = Left err
getResult (Success succ) = Right succ
