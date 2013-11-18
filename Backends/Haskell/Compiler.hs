module Backends.Haskell.Compiler where

import MatchIExpr
import Syntax

import Backends.Haskell.Syntax

import Control.Applicative (liftA)
import Data.Either (partitionEithers)


--------------- HASMOD Compilation --------------

iModule = liftA (hsMod . partitionEithers . snd) iModuleExpr
  where
    iModuleExpr = mTreeSep' (msimplegroup ["hasmod"]) ":"
                            (lMany iModuleBodyStatement)
    iModuleBodyStatement = liftA Left iImport `orTry` liftA Right iDefinition
    hsMod (importGroups, definitions) = HSMod (concat importGroups) definitions

iImport = liftA hsImport mImport
 where
   mImport = mTreeSep' (msimplegroup ["import"]) ":" (lMany . mGroup $ mModulePath)
   mModulePath = lMany (mAtom mAny)
   hsImport (_,modules) = map HSImport modules

iDefinition = iDefFunc `orTry` iDefData

iDefFunc = liftA (HSFuncDef . snd) mFuncDef
 where
   mFuncDef = mTreeSep' (msimplegroup ["define"]) ":" (lMany iDecl)

iDefData = liftA datadef mDataDef
 where
   mDataDef = mTreeSep' (mGroup (mAtom (mEq "define-data") `lCons` lMany (mAtom mAny))) ":"
                        (lMany iDataConstr)
   datadef ((_, name : typevars), constructors) = HSDataDef name typevars constructors

iDataConstr = liftA dataconstr mDataConstr
 where
   mDataConstr = msomegroup (mAtom mAny `lCons` lMany iType)
   dataconstr (name,types) = HSDataConstr name types

iType = liftA hsType mType `orTry` liftA HSTypeVar mTypeVar
 where
   mType = msingle (mList Parens (mAtom mAny `lCons` lMany iType))
           `orTry` mGroup (mAtom mAny `lCons` lMany1 iType)
   mTypeVar = msingle (mAtom mAny)

   hsType (name, subtypes) = HSType name subtypes


iDecl = liftA declaration $ mTreeSep' iSignature ":=" (lList [iExpr])
 where
   iSignature = mGroup $ mAtom mAny `lCons` lMany iPat
   declaration ((name,args), [body]) = HSDecl name args body

iPat = liftA PVar $ mAtom mAny

iExpr = iAppGroup `orTry` iAppTree `orTry` iString `orTry` iVariable
 where
   iAppGroup = liftA (foldl1 HSApp) $ msomegroup (lMany iExpr)

   iAppTree  = liftA (uncurry (foldl HSApp)) (mSimpleTree `orTry` mAppSepTree)
   mSimpleTree = mTree iExpr (lMany iExpr)
   mAppSepTree = mTreeSep' iExpr ":-" (lMany iExpr)

   iString   = liftA HSString $ mString mAny

   iVariable = liftA HSVar $ mAtom mAny


-- this function name is very bad
mTreeSep' mroot seperator mbody = liftA extractTree $ mTreeSep mroot (mEq seperator) mbody
 where
   extractTree (root,seperator,body) = (root,body)

msomegroup msub = mGroup msub `orTry` mList Parens msub
msimplegroup atomnames = mGroup . lList . map (mAtom . mEq) $ atomnames

msingle msub = liftA head (mGroup (lList [msub])) `orTry` msub

