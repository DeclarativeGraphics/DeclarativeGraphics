module Backends.Haskell.Compiler where

import Compilation
import MatchIExpr
import Syntax

import Backends.Haskell.Syntax

import Control.Applicative (liftA)
import Data.Either (partitionEithers)


--------------- HASMOD Compilation --------------

iModule = liftA (hsMod . partitionEithers . snd) . iModuleExpr
  where
    iModuleExpr = mtreesep' (msimplegroup ["hasmod"]) ":"
                            (lmany iModuleBodyStatement)
    iModuleBodyStatement = (liftA Left . iImport) `orTry` (liftA Right . iDefinition)
    hsMod (importGroups, definitions) = HSMod (concat importGroups) definitions

iImport = liftA hsImport . mImport
 where
   mImport = mtreesep' (msimplegroup ["import"]) ":" (lmany . mgroup $ mModulePath)
   mModulePath = lmany (matom mAny)
   hsImport (_,modules) = map HSImport modules

iDefinition = iDefFunc `orTry` iDefData

iDefFunc = liftA (HSFuncDef . snd) . mFuncDef
 where
   mFuncDef = mtreesep' (msimplegroup ["define"]) ":" (lmany iDecl)

iDefData = liftA datadef . mDataDef
 where
   mDataDef = mtreesep' (mgroup (matom (meq "define-data") `lcons` lmany (matom mAny))) ":"
                        (lmany iDataConstr)
   datadef ((_, name : typevars), constructors) = HSDataDef name typevars constructors

iDataConstr = liftA dataconstr . mDataConstr
 where
   mDataConstr = msomegroup (matom mAny `lcons` lmany iType)
   dataconstr (name,types) = HSDataConstr name types

iType = (liftA hsType . mType) `orTry` (liftA HSTypeVar . mTypeVar)
 where
   mType = msingle (mlist Parens (matom mAny `lcons` lmany iType))
           `orTry` mgroup (matom mAny `lcons` lmany1 iType)
   mTypeVar = msingle (matom mAny)

   hsType (name, subtypes) = HSType name subtypes


iDecl = liftA declaration . mtreesep' iSignature ":=" (llist [iExpr])
 where
   iSignature = mgroup $ matom mAny `lcons` lmany iPat
   declaration ((name,args), [body]) = HSDecl name args body

iPat = liftA PVar . matom mAny

iExpr = iAppGroup `orTry` iAppTree `orTry` iString `orTry` iVariable
 where
   iAppGroup = liftA (foldl1 HSApp) . msomegroup (lmany iExpr)

   iAppTree  = liftA (uncurry (foldl HSApp)) . (mSimpleTree `orTry` mAppSepTree)
   mSimpleTree = mtree iExpr (lmany iExpr)
   mAppSepTree = mtreesep' iExpr ":-" (lmany iExpr)

   iString   = liftA HSString . mstring mAny

   iVariable = liftA HSVar . matom mAny


-- this function name is very bad
mtreesep' mroot seperator mbody = liftA extractTree . mtreesep mroot (meq seperator) mbody
 where
   extractTree (root,seperator,body) = (root,body)

msomegroup msub = mgroup msub `orTry` mlist Parens msub
msimplegroup atomnames = mgroup . llist . map (matom . meq) $ atomnames

msingle msub = (liftA head . mgroup (llist [msub])) `orTry` msub

