module Backends.Haskell.Compiler where

import IExpr
import MatchIExpr

import Backends.Haskell.Syntax

import Control.Applicative (liftA)
import Data.Either (partitionEithers)


--------------- HASMOD Compilation --------------

iModule = liftA (hsMod . partitionEithers . snd) iModuleExpr
  where
    iModuleExpr = mTreeSep' (msimplelist ["hasmod"]) ":"
                            (lMany iModuleBodyStatement)
    iModuleBodyStatement = liftA Left iImport `orTry` liftA Right iDefinition
    hsMod (importGroups, definitions) = HSMod (concat importGroups) definitions

iImport = liftA hsImport mImport
 where
   mImport = mTreeSep' (msimplelist ["import"]) ":" (lMany . mList $ mModulePath)
   mModulePath = lMany (mAtom mAny)
   hsImport (_,modules) = map HSImport modules

iDefinition = iDefFunc `orTry` iDefData

iDefFunc = liftA (HSFuncDef . snd) $ mLineGroup (mAtomEq "define") (lMany iDecl)

iDefData = liftA datadef mDataDef
 where
   mDataDef = mTreeSep' (mList (mAtomEq "define-data" `lCons` lMany mAnyAtom)) ":"
                        (lMany iDataConstr)
   datadef ((_, name : typevars), constructors) = HSDataDef name typevars constructors

iDataConstr = liftA dataconstr mDataConstr
 where
   mDataConstr = mList (mAnyAtom `lCons` lMany iType)
   dataconstr (name,types) = HSDataConstr name types

iType = liftA hsType mType `orTry` liftA HSTypeVar mTypeVar
 where
   mType = mList (mAnyAtom `lCons` lMany1 iType)
   mTypeVar = msingle mAnyAtom

   hsType (name, subtypes) = HSType name subtypes


iDecl = liftA declaration $ mTreeSep' iSignature ":=" (lList [iExpr])
 where
   iSignature = mList $ mAnyAtom `lCons` lMany iPat
   declaration ((name,args), [body]) = HSDecl name args body

iPat = liftA PVar mAnyAtom

iExpr = iAppList `orTry` iAppTree `orTry` iString `orTry` iVariable
 where
   iAppList = liftA (foldl1 HSApp) $ mList (lMany iExpr)

   iAppTree  = liftA hsApp (mSimpleTree `orTry` mAppSepTree)
   mSimpleTree = mTree iExpr (lMany iExpr)
   mAppSepTree = mTreeSep' iExpr ":-" (lMany iExpr)

   hsApp (func,args) = foldl HSApp func args

   iString   = liftA HSString $ mString mAny

   iVariable = liftA HSVar mAnyAtom



mLineGroup mroot mlines = liftA extractTreeLines treeLines
                  `orTry` liftA extractBraceLine oneBraceLine
 where
   treeLines = mTreeSep' (mList $ lList [mroot]) ":" mlines
   extractTreeLines ([root], sublines) = (root, sublines)

   oneBraceLine = mList (mroot `lCons` lList [mBraceList mlines])
   extractBraceLine (root, [sublines]) = (root, sublines)


-- this function name is very bad
mTreeSep' mroot seperator mbody = liftA extractTree $ mTreeSep mroot (mEq seperator) mbody
 where
   extractTree (root,seperator,body) = (root,body)

mAnyAtom = mAtom mAny

msimplelist atomnames = mList . lList . map mAtomEq $ atomnames

mAtomEq = mAtom . mEq

msingle msub = liftA head (mList (lList [msub])) `orTry` msub
