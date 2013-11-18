module MatchIExpr (
    lMany,
    lMany1,
    lList,
    lCons,

    mAny,
    mSatisfy,
    mEq,

    IExprMatcher,
    IExprListMatcher,
    IExprError,

    ContextualError(..),
    sourceError,
    simpleError,
    withContext,

    mAtom,
    mString,
    mGroup,
    mList,
    mDecor,
    mTree,
    mTreeSep,
    mHash,

    orTry
    ) where


import Matching
import Syntax

import Control.Applicative (Applicative(..),liftA,liftA2,liftA3)
import Data.Monoid (Monoid(mappend))


-------------- IExpr Matching ---------------

type IExprMatcher     = Matcher IExpr IExprError
type IExprListMatcher = Matcher [IExpr] IExprError
type IExprError       = [ContextualError Message IExpr]

data ContextualError c a = ErrorContext c (ContextualError c a)
                         | ErrorSource Message a
                         | ErrorMessage Message
                         deriving Show

type Message = String


sourceError :: Message -> IExpr -> Result IExprError a
sourceError message source = Error [ErrorSource message source]

simpleError :: Message -> Result IExprError a
simpleError message        = Error [ErrorMessage message]

withContext :: c -> Result [ContextualError c s] a -> Result [ContextualError c s] a
withContext context (Success s) = Success s
withContext context (Error e)   = Error $ map (ErrorContext context) e



------------- List Matcher -------------


llist :: [i -> Result IExprError a] -> [i] -> Result IExprError [a]
llist []     []     = pure []
llist (m:ms) (x:xs) = liftA2 (:) (m x) (llist ms xs)
llist []     _      = simpleError "too many elements"
llist _      []     = simpleError "too few elements"

_cons :: (i -> Result IExprError a)
      -> ([i] -> Result IExprError b)
      -> [i] -> Result IExprError (a,b)
_cons mhead mtail (x:xs) = liftA2 (,) (mhead x) (mtail xs)
_cons _     _     []     = simpleError "expected non-empty list"



lMany :: Matcher i IExprError a -> Matcher [i] IExprError [a]
lMany melem = M $ foldr (liftA2 (:) . runMatch melem) (pure [])

lMany1 :: Matcher i IExprError a -> Matcher [i] IExprError [a]
lMany1 msub = liftA (uncurry (:)) (msub `lCons` lMany msub)

lList :: [Matcher i IExprError a] -> Matcher [i] IExprError [a]
lList = M . llist . map runMatch

lCons :: Matcher i IExprError a -> Matcher [i] IExprError b -> Matcher [i] IExprError (a,b)
lCons = transform2 _cons



------------ IExpr Matcher ------------

_Any :: Monoid e => i -> Result e i
_Any = pure

_satisfy pred x | pred x    = pure x
                | otherwise = sourceError "unexpected" x

_eq x input | input == x = pure input
            | otherwise  = simpleError $ "expected " ++ show x ++ " but got " ++ show input

_atom mname (IAtom name) = withContext "atom" $ mname name
_atom mname expr         = sourceError "atom" expr

_string mcont (IString cont) = withContext "string" $ mcont cont
_string mcont expr           = sourceError "string" expr

_list mlisttype melems (IList listtype elems)
  | mlisttype == listtype   = withContext "list" $ melems elems
_list _         _      expr = sourceError "list" expr

_group melems (IGroup elems) = withContext "group" $ melems elems
_group melems expr           = sourceError "group" expr


_decor mdecorator mdecorated (IDecor decorator decorated)
  = getBoth (withContext "decorator" $ mdecorator decorator)
            (withContext "decorated" $ mdecorated decorated)
_decor _ _ expr = sourceError "decor" expr


_tree mroot mleaves (ITree root leaves)
  = withContext "tree" $
      getBoth (withContext "root"   $ mroot root)
              (withContext "leaves" $ mleaves leaves)
_tree _ _ expr = sourceError "tree" expr


_treesep mroot msep mleaves (ITreeSep sep root leaves)
  = withContext "treesep" $
      liftA3 (,,) (withContext "root"   $ mroot root)
                  (withContext "sep"    $ msep (':' : sep))
                  (withContext "leaves" $ mleaves leaves)
_treesep _ _ _ expr = sourceError "treesep" expr


_hash mexpr mcontent (IHash expr content)
  = withContext "hash" $ getBoth (withContext "expr"    $ mexpr expr)
                                 (withContext "content" $ mcontent content)
_hash _ _ expr = sourceError "hash" expr




-------------------- MATCHERS --------------------


mAny :: Monoid e => Matcher i e i
mAny = M _Any

mSatisfy :: (IExpr -> Bool) -> IExprMatcher IExpr
mSatisfy pred = M $ _satisfy pred

mEq :: (Eq i, Show i) => i -> Matcher i IExprError i
mEq x = M $ _eq x

mAtom :: Matcher String IExprError a -> IExprMatcher a
mAtom = transform1 _atom

mString :: Matcher String IExprError a -> IExprMatcher a
mString = transform1 _string

mList :: ParenType -> IExprListMatcher a -> IExprMatcher a
mList mlisttype mElems = M . _list mlisttype $ runMatch mElems

mGroup :: IExprListMatcher a -> IExprMatcher a
mGroup = transform1 _group

mDecor :: IExprMatcher a -> IExprMatcher b -> IExprMatcher (a,b)
mDecor = transform2 _decor

mTree :: IExprMatcher a -> IExprListMatcher b -> IExprMatcher (a,b)
mTree = transform2 _tree

mTreeSep :: IExprMatcher r
         -> Matcher String IExprError s
         -> IExprListMatcher b
         -> IExprMatcher (r,s,b)
mTreeSep = transform3 _treesep

mHash :: IExprMatcher a -> Matcher [String] IExprError b -> IExprMatcher (a,b)
mHash = transform2 _hash



----------------- COMBINATORS ----------------


(M left) `orTry` (M right) = M $ \input -> case left input of
  Success success -> Success success
  Error errorleft -> case right input of
    Success success -> Success success
    Error errorright -> Error $ errorleft `mappend` errorright



----------------- HELPERS ----------------

getBoth = liftA2 (,)
