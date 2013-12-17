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
   mModulePath = lMany (mSymbol mAny)
   hsImport (_,modules) = map HSImport modules

iDefinition = iDefFunc `orTry` iDefData

iDefFunc = liftA (HSFuncDef . snd) $ mLineGroup (mSymbolEq "define") (lMany iDecl)

iDefData = liftA datadef mDataDef
 where
   mDataDef = mTreeSep' (mList (mSymbol (mEq "define-data") `lCons` lMany mAnySymbol)) ":"
                        (lMany iDataConstr)
   datadef ((_, name : typevars), constructors) = HSDataDef name typevars constructors

iDataConstr = liftA dataconstr mDataConstr
 where
   mDataConstr = msomelist (mAnySymbol `lCons` lMany iType)
   dataconstr (name,types) = HSDataConstr name types

iType = liftA hsType mType `orTry` liftA HSTypeVar mTypeVar
 where
   mType = msingle (mGroup Parens (mAnySymbol `lCons` lMany iType))
           `orTry` mList (mAnySymbol `lCons` lMany1 iType)
   mTypeVar = msingle mAnySymbol

   hsType (name, subtypes) = HSType name subtypes


iDecl = liftA declaration $ mTreeSep' iSignature ":=" (lList [iExpr])
 where
   iSignature = mList $ mAnySymbol `lCons` lMany iPat
   declaration ((name,args), [body]) = HSDecl name args body

iPat = liftA PVar mAnySymbol

iExpr = iAppList `orTry` iAppTree `orTry` iString `orTry` iVariable
 where
   iAppList = liftA (foldl1 HSApp) $ msomelist (lMany iExpr)

   iAppTree  = liftA hsApp (mSimpleTree `orTry` mAppSepTree)
   mSimpleTree = mTree iExpr (lMany iExpr)
   mAppSepTree = mTreeSep' iExpr ":-" (lMany iExpr)

   hsApp (func,args) = foldl HSApp func args

   iString   = liftA HSString $ mString mAny

   iVariable = liftA HSVar mAnySymbol



mLineGroup mroot mlines = liftA extractTreeLines treeLines
                  `orTry` liftA extractBraceLine oneBraceLine
 where
   treeLines = mTreeSep' (mList $ lList [mroot]) ":" mlines
   extractTreeLines ([root], sublines) = (root, sublines)

   oneBraceLine = msomelist (mroot `lCons` lList [mGroup Braces mlines])
   extractBraceLine (root, [sublines]) = (root, sublines)


-- this function name is very bad
mTreeSep' mroot seperator mbody = liftA extractTree $ mTreeSep mroot (mEq seperator) mbody
 where
   extractTree (root,seperator,body) = (root,body)

mAnySymbol = mSymbol mAny

msomelist melems = mList melems `orTry` mGroup Parens melems
msimplelist atomnames = mList . lList . map mSymbolEq $ atomnames

mSymbolEq = mSymbol . mEq

msingle msub = liftA head (mList (lList [msub])) `orTry` msub
