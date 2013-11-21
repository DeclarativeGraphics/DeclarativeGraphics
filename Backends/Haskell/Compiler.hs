module Backends.Haskell.Compiler where

import IExpr
import MatchIExpr

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
   mModulePath = lMany (mSymbol mAny)
   hsImport (_,modules) = map HSImport modules

iDefinition = iDefFunc `orTry` iDefData

iDefFunc = liftA (HSFuncDef . snd) mFuncDef
 where
   mFuncDef = mTreeSep' (msimplegroup ["define"]) ":" (lMany iDecl)

iDefData = liftA datadef mDataDef
 where
   mDataDef = mTreeSep' (mGroup (mSymbol (mEq "define-data") `lCons` lMany mAnySymbol)) ":"
                        (lMany iDataConstr)
   datadef ((_, name : typevars), constructors) = HSDataDef name typevars constructors

iDataConstr = liftA dataconstr mDataConstr
 where
   mDataConstr = msomegroup (mAnySymbol `lCons` lMany iType)
   dataconstr (name,types) = HSDataConstr name types

iType = liftA hsType mType `orTry` liftA HSTypeVar mTypeVar
 where
   mType = msingle (mList Parens (mAnySymbol `lCons` lMany iType))
           `orTry` mGroup (mAnySymbol `lCons` lMany1 iType)
   mTypeVar = msingle mAnySymbol

   hsType (name, subtypes) = HSType name subtypes


iDecl = liftA declaration $ mTreeSep' iSignature ":=" (lList [iExpr])
 where
   iSignature = mGroup $ mAnySymbol `lCons` lMany iPat
   declaration ((name,args), [body]) = HSDecl name args body

iPat = liftA PVar mAnySymbol

iExpr = iAppGroup `orTry` iAppTree `orTry` iString `orTry` iVariable
 where
   iAppGroup = liftA (foldl1 HSApp) $ msomegroup (lMany iExpr)

   iAppTree  = liftA hsApp (mSimpleTree `orTry` mAppSepTree)
   mSimpleTree = mTree iExpr (lMany iExpr)
   mAppSepTree = mTreeSep' iExpr ":-" (lMany iExpr)

   hsApp (func,args) = foldl HSApp func args

   iString   = liftA HSString $ mString mAny

   iVariable = liftA HSVar mAnySymbol


-- this function name is very bad
mTreeSep' mroot seperator mbody = liftA extractTree $ mTreeSep mroot (mEq seperator) mbody
 where
   extractTree (root,seperator,body) = (root,body)

mAnySymbol = mSymbol mAny

msomegroup msub = mGroup msub `orTry` mList Parens msub
msimplegroup atomnames = mGroup . lList . map (mSymbol . mEq) $ atomnames

msingle msub = liftA head (mGroup (lList [msub])) `orTry` msub

