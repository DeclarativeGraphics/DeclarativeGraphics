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
    matcherError,
    withContext,
    contextual,
    contextualMatcher,
    showContextualErrors,

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
import Data.List (intersperse)
import Data.Monoid (Monoid(mappend))
import Data.Maybe (mapMaybe)


-------------- IExpr Matching ---------------

type IExprMatcher s     = Matcher (IExpr s) (IExprError s)
type IExprListMatcher s = Matcher [IExpr s] (IExprError s)
type IExprError s       = [ContextualError (IExpr s)]

data SimpleError s e = SErrorSource s Message e
                     | SErrorMessage s Message
                     deriving Show

data ContextualError e = CErrorContext e (ContextualError e)
                       | CErrorContextMessage Message (ContextualError e)
                       | CErrorMessage Message

type Message = String


showContextualErrors count err
  = unlines . intersperse " -- OR --" . map (showError . errorTrace) . take count $ err
 where
   showError (message, trace) = message ++ " " ++ showTrace trace

   showTrace = unwords . mapMaybe showErrorContext . takeUpTo isCErrorContext . reverse

   takeUpTo pred [] = []
   takeUpTo pred (x:xs) | pred x    = [x]
                        | otherwise = x : takeUpTo pred xs

   isCErrorContext (CErrorContext _ _) = True
   isCErrorContext _                   = False

   showErrorContext (CErrorContextMessage message _)
     = return message
   showErrorContext (CErrorContext surroundingExpression _)
     = do position <- getPosition surroundingExpression
          return $ "in " ++ show position

   errorTrace (CErrorMessage message) = (message, [])
   errorTrace context@(CErrorContextMessage _ innerError)
     = let (message, trace) = errorTrace innerError
       in (message, context : trace)
   errorTrace context@(CErrorContext _ innerError)
     = let (message, trace) = errorTrace innerError
       in (message, context : trace)


matcherError :: Message -> Result (IExprError pos) a
matcherError message = Error [CErrorMessage message]

contextual :: (IExpr pos -> Result (IExprError pos) a)
           -> IExpr pos
           -> Result (IExprError pos) a
contextual matcher input = withContext input (matcher input)

withContext :: IExpr pos -> Result (IExprError pos) a -> Result (IExprError pos) a
withContext context (Success s) = Success s
withContext context (Error e)   = Error $ map (CErrorContext context) e

contextualMatcher matcher = M $ contextual matcher


match `inContext` context = M $ withContextMessage context . runMatcher match

withContextMessage context (Success s) = Success s
withContextMessage context (Error e)   = Error $ map (CErrorContextMessage context) e



------------- List Matcher -------------


lMany :: IExprMatcher pos a -> IExprListMatcher pos [a]
lMany matchElem = M $ foldr consMatch (pure [])
 where
   consMatch input rest = withContext input $ liftA2 (:) (runMatcher matchElem input) rest

lMany1 :: IExprMatcher pos a -> IExprListMatcher pos [a]
lMany1 msub = liftA (uncurry (:)) (msub `lCons` lMany msub)

lList :: [IExprMatcher pos a] -> IExprListMatcher pos [a]
lList matchElems = M $ matchList matchElems
 where
   matchList (matchElem:matchOther) (elem:other)
                   = withContext elem $ liftA2 (:) (runMatcher matchElem elem)
                                                   (matchList matchOther other)
   matchList [] [] = pure []
   matchList [] _  = matcherError "too many elements"
   matchList _  [] = matcherError "too few elements"

infixr `lCons`
lCons :: IExprMatcher pos a
      -> IExprListMatcher pos b
      -> IExprListMatcher pos (a,b)
lCons matchHead matchTail = M matchCons
 where
   matchCons (head:tail) = withContext head $
                             getBoth (runMatcher matchHead head)
                                     (runMatcher matchTail tail)
   matchCons []          = matcherError "expected non-empty list"


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
     Nothing     -> matcherError $ "unexpected " ++ show input

mSatisfy :: (IExpr s -> Bool) -> IExprMatcher s (IExpr s)
mSatisfy pred = contextualMatcher satisfyMatcher
 where
   satisfyMatcher input | pred input = pure input
                        | otherwise  = matcherError "unexpected"

mEq :: (Eq i, Show i) => i -> Matcher i (IExprError s) i
mEq expect = M eqMatcher
 where
   eqMatcher input | input == expect = pure input
                   | otherwise       = matcherError $ "unexpected " ++ show input

mSymbol :: Matcher String (IExprError s) a -> IExprMatcher s a
mSymbol matchName = contextualMatcher symbolMatcher
 where
   symbolMatcher input = case input of
     ISymbol name _ -> runMatcher matchName name
     otherwise      -> matcherError "expected symbol"

mString :: Matcher String (IExprError s) a -> IExprMatcher s a
mString matchContents = contextualMatcher matchString
 where
   matchString input = case input of
     IString contents _ -> runMatcher matchContents contents
     otherwise          -> matcherError "expected String"

mGroup :: ParenType -> IExprListMatcher s a -> IExprMatcher s a
mGroup expectedparentype matchElems = contextualMatcher matchList
 where
   matchList input = case input of
     IGroup parentype elems -> if parentype == expectedparentype
                               then runMatcher matchElems elems
                               else matcherError $
                                      "expected parentype " ++ show expectedparentype
     otherwise -> matcherError "expected group"

mList :: IExprListMatcher s a -> IExprMatcher s a
mList matchElems = contextualMatcher matchList
 where
   matchList input = case input of
     IList elems -> runMatcher matchElems elems
     otherwise   -> matcherError "expected list"

mDecor :: IExprMatcher s a -> IExprMatcher s b -> IExprMatcher s (a,b)
mDecor matchDecorator matchDecorated = contextualMatcher matchDecor
 where
   matchDecor input = case input of
     IDecor decorator decorated -> getBoth (runMatcher matchDecorator decorator)
                                           (runMatcher matchDecorated decorated)
     otherwise -> matcherError "expected decorator"

mTree :: IExprMatcher s a -> IExprListMatcher s b -> IExprMatcher s (a,b)
mTree matchRoot matchLeaves = contextualMatcher matchTree
 where
   matchTree input = case input of
     ITree root leaves -> getBoth (runMatcher matchRoot root)
                                  (withContextMessage "in body" $
                                     runMatcher matchLeaves leaves)
     otherwise -> matcherError "expected tree"

mTreeSep :: IExprMatcher s r
         -> Matcher String (IExprError s) sep
         -> IExprListMatcher s b
         -> IExprMatcher s (r,sep,b)
mTreeSep matchRoot matchSeperator matchLeaves = contextualMatcher matchTreeSep
 where
   matchTreeSep input = case input of
     ITreeSep (seperator, _) root leaves
       -> liftA3 (,,) (runMatcher matchRoot root)
                      (runMatcher matchSeperator (':' : seperator))
                      (withContextMessage ("right of :" ++ seperator) $
                         runMatcher matchLeaves leaves)
     otherwise -> matcherError "expected tree with seperator"

mHash :: IExprMatcher s a -> Matcher [String] (IExprError s) b -> IExprMatcher s (a,b)
mHash matchRoot matchContents = contextualMatcher matchHash
 where
   matchHash input = case input of
     IHash root contents -> getBoth (runMatcher matchRoot root)
                                    (runMatcher matchContents contents)
     otherwise -> matcherError "expected hash"



----------------- COMBINATORS ----------------


(M left) `orTry` (M right) = M $ \input -> case left input of
  Success success -> Success success
  Error errorleft -> case right input of
    Success success -> Success success
    Error errorright -> Error $ errorleft `mappend` errorright



----------------- HELPERS ----------------

getBoth :: Applicative f => f a -> f b -> f (a, b)
getBoth = liftA2 (,)
