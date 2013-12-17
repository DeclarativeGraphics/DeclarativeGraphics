module MatchIExpr (
    lMany,
    lMany1,
    lList,
    lCons,

    mAny,
    mTry,
    mSatisfy,
    mEq,

    IExprMatcher,
    IExprListMatcher,
    IExprError,

    SimpleError(..),
    ContextualError(..),
    sourceError,
    simpleError,
    simplePosError,
    withContext,

    mSymbol,
    mString,
    mGroup,
    mList,
    mDecor,
    mTree,
    mTreeSep,
    mHash,

    orTry
    ) where


import IExpr
import Matching

import Control.Applicative (Applicative(..),liftA,liftA2,liftA3)
import Data.Monoid (Monoid(mappend))


-------------- IExpr Matching ---------------

type IExprMatcher s     = Matcher (IExpr s) (IExprError s)
type IExprListMatcher s = Matcher [IExpr s] (IExprError s)
type IExprError s       = [SimpleError (Maybe s) (IExpr s)]

data SimpleError s e = SErrorSource s Message e
                     | SErrorMessage s Message
                     deriving Show

data ContextualError s c e = CErrorContext s c (ContextualError s c e)
                           | CErrorSource s Message e
                           | CErrorMessage s Message
                           deriving Show

type Message = String


sourceError :: Message -> IExpr pos -> Result (IExprError pos) a
sourceError message source = Error [SErrorSource (getPosition source) message source]

simpleError :: Message -> Result (IExprError pos) a
simpleError message = Error [SErrorMessage Nothing message]

simplePosError :: Message -> pos -> Result (IExprError pos) a
simplePosError message pos = Error [SErrorMessage (Just pos) message]

withContext :: c -> Result [ContextualError s c e] a -> s -> Result [ContextualError s c e] a
withContext context (Success s) pos = Success s
withContext context (Error e)   pos = Error $ map (CErrorContext pos context) e



------------- List Matcher -------------


lMany :: Matcher i (IExprError s) a -> Matcher [i] (IExprError s) [a]
lMany matchElem = M $ foldr consMatch (pure [])
 where
   consMatch input rest = liftA2 (:) (runMatcher matchElem input) rest

lMany1 :: Matcher i (IExprError s) a -> Matcher [i] (IExprError s) [a]
lMany1 msub = liftA (uncurry (:)) (msub `lCons` lMany msub)

lList :: [Matcher i (IExprError s) a] -> Matcher [i] (IExprError s) [a]
lList matchElems = M $ matchList matchElems
 where
   matchList (matchElem:matchOther) (elem:other) = liftA2 (:) (runMatcher matchElem elem)
                                                              (matchList matchOther other)
   matchList [] [] = pure []
   matchList [] _  = simpleError "too many elements"
   matchList _  [] = simpleError "too few elements"

infixr `lCons`
lCons :: Matcher i (IExprError s) a -> Matcher [i] (IExprError s) b -> Matcher [i] (IExprError s) (a,b)
lCons matchHead matchTail = M matchCons
 where
   matchCons (head:tail) = getBoth (runMatcher matchHead head)
                                   (runMatcher matchTail tail)
   matchCons []          = simpleError "expected non-empty list"


-------------------- MATCHERS --------------------


mAny :: Monoid e => Matcher i e i
mAny = M anyMatcher
 where
   anyMatcher x = pure x

mTry :: Show i => (i -> Maybe b) -> Matcher i (IExprError s) b
mTry func = M tryMatcher
 where
   tryMatcher input = case func input of
     Just result -> pure result
     Nothing     -> simpleError $ "unexpected " ++ show input

mSatisfy :: (IExpr s -> Bool) -> IExprMatcher s (IExpr s)
mSatisfy pred = M satisfyMatcher
 where
   satisfyMatcher input | pred input = pure input
                        | otherwise  = sourceError "unexpected" input

mEq :: (Eq i, Show i) => i -> Matcher i (IExprError s) i
mEq expect = M eqMatcher
 where
   eqMatcher input | input == expect = pure input
                   | otherwise       = simpleError $ "unexpected " ++ show input

mSymbol :: Matcher String (IExprError s) a -> IExprMatcher s a
mSymbol matchName = M symbolMatcher
 where
   symbolMatcher input = case input of
     ISymbol name _ -> runMatcher matchName name
     otherwise      -> sourceError "expected symbol" input

mString :: Matcher String (IExprError s) a -> IExprMatcher s a
mString matchContents = M matchString
 where
   matchString input = case input of
     IString contents _ -> runMatcher matchContents contents
     otherwise          -> sourceError "expected String" input

mGroup :: ParenType -> IExprListMatcher s a -> IExprMatcher s a
mGroup expectedparentype matchElems = M matchList
 where
   matchList input = case input of
     IGroup parentype elems -> if parentype == expectedparentype
                               then runMatcher matchElems elems
                               else sourceError "parentype doesn't match" input
     otherwise -> sourceError "expected group" input

mList :: IExprListMatcher s a -> IExprMatcher s a
mList matchElems = M matchList
 where
   matchList input = case input of
     IList elems -> runMatcher matchElems elems
     otherwise   -> sourceError "expected list" input

mDecor :: IExprMatcher s a -> IExprMatcher s b -> IExprMatcher s (a,b)
mDecor matchDecorator matchDecorated = M matchDecor
 where
   matchDecor input = case input of
     IDecor decorator decorated -> getBoth (runMatcher matchDecorator decorator)
                                           (runMatcher matchDecorated decorated)
     otherwise -> sourceError "expected decorator" input

mTree :: IExprMatcher s a -> IExprListMatcher s b -> IExprMatcher s (a,b)
mTree matchRoot matchLeaves = M matchTree
 where
   matchTree input = case input of
     ITree root leaves -> getBoth (runMatcher matchRoot root)
                                  (runMatcher matchLeaves leaves)
     otherwise -> sourceError "expected tree" input

mTreeSep :: IExprMatcher s r
         -> Matcher String (IExprError s) sep
         -> IExprListMatcher s b
         -> IExprMatcher s (r,sep,b)
mTreeSep matchRoot matchSeperator matchLeaves = M matchTreeSep
 where
   matchTreeSep input = case input of
     ITreeSep (seperator, _) root leaves
       -> liftA3 (,,) (runMatcher matchRoot root)
                      (runMatcher matchSeperator (':' : seperator))
                      (runMatcher matchLeaves leaves)
     otherwise -> sourceError "expected tree with seperator" input

mHash :: IExprMatcher s a -> Matcher [String] (IExprError s) b -> IExprMatcher s (a,b)
mHash matchRoot matchContents = M matchHash
 where
   matchHash input = case input of
     IHash root contents -> getBoth (runMatcher matchRoot root)
                                    (runMatcher matchContents contents)
     otherwise -> sourceError "expected hash" input



----------------- COMBINATORS ----------------


(M left) `orTry` (M right) = M $ \input -> case left input of
  Success success -> Success success
  Error errorleft -> case right input of
    Success success -> Success success
    Error errorright -> Error $ errorleft `mappend` errorright



----------------- HELPERS ----------------

getBoth :: Applicative f => f a -> f b -> f (a, b)
getBoth = liftA2 (,)
