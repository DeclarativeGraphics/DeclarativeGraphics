module MatchIExpr where

import Syntax
import Compilation

import Control.Applicative (Applicative(..),liftA3)


-------------- IExpr Matching ---------------


mAny x = pure x

msatisfy pred x | pred x    = pure x
                | otherwise = compileError "unexpected" x

meq x input | input == x = pure input
            | otherwise  = simpleError $ "expected " ++ show x ++ " but got " ++ show input

matom mname (IAtom name) = withContext "atom" $ mname name
matom mname expr         = compileError "atom" expr

mstring mcont (IString cont) = withContext "string" $ mcont cont
mstring mcont expr           = compileError "string" expr

mlist mlisttype melems (IList listtype elems)
  | mlisttype == listtype   = withContext "list" $ melems elems
mlist _         _      expr = compileError "list" expr

mgroup melems (IGroup elems) = withContext "group" $ melems elems
mgroup melems expr           = compileError "group" expr


mdecor mdecorator mdecorated (IDecor decorator decorated)
  = getBoth (withContext "decorator" $ mdecorator decorator)
            (withContext "decorated" $ mdecorated decorated)
mdecor _          _          expr
  = compileError "decor" expr


mtree mroot mleaves (ITree root leaves)
  = withContext "tree" $
      getBoth (withContext "root"   $ mroot root)
              (withContext "leaves" $ mleaves leaves)
mtree _     _       expr
  = compileError "tree" expr


mtreesep mroot msep mleaves (ITreeSep sep root leaves)
  = withContext "treesep" $
      liftA3 (,,) (withContext "root"   $ mroot root)
                  (withContext "sep"    $ msep (':' : sep))
                  (withContext "leaves" $ mleaves leaves)
mtreesep _     _    _       expr
  = compileError "treesep" expr


mhash mexpr mcontent (IHash expr content)
  = withContext "hash" $ getBoth (withContext "expr"    $ mexpr expr)
                                 (withContext "content" $ mcontent content)
mhash _     _        expr
  = compileError "hash" expr
