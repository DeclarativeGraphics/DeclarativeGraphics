module HaskellBackend where

import Syntax

import Control.Monad (guard,liftM,liftM2,mplus,zipWithM)
import Data.List (intersperse,sortBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Text.PrettyPrint
import System.FilePath (takeBaseName)


type Name = String
type Var = String

data HSMod = HSMod [HSImport] [HSDef] deriving Show
data HSImport = HSImport [String] deriving Show
data HSDef = HSFuncDef [HSDecl]
           | HSDataDef Name [Var] [HSDataConstr] deriving Show
data HSDataConstr = HSDataConstr Name [HSType] deriving Show
data HSType = HSType Name [HSType] | HSTypeVar Var deriving Show
data HSDecl = HSDecl Name [HSPat] HSExpr deriving Show
data HSPat = PVar Var deriving Show
data HSExpr = HSApp HSExpr HSExpr | HSVar Var | HSString String | HSInt Int
            | HSOp Name deriving Show

prettyMod modname (HSMod imports definitions)
  = text "module" <+> text modname <+> text "where" $+$
    emptyLine $+$
    (vcat . map prettyImport . sortBy (comparing moduleparts) $ imports) $+$
    emptyLine $+$
    (vcat . intersperse emptyLine $ map prettyDefinition definitions)
 where
   emptyLine = text ""
   moduleparts (HSImport modparts) = modparts

prettyImport (HSImport modparts)
  = text "import" <+> (hcat . punctuate (char '.') $ map text modparts)

prettyDefinition (HSFuncDef declarations)
  = vcat $ map prettyDeclaration declarations
prettyDefinition (HSDataDef typename typeargs constructors)
  = text "data" <+> text typename <+> hsep (map text typeargs) <+> text "=" <+>
    (sep . map (text "|" <+>) . map prettyDataConstr $ constructors)

prettyDataConstr (HSDataConstr name subtypes)
  = text name <+> sep (map (parens . prettyType) subtypes)

prettyType (HSType typename subtypes)
  = text typename <+> sep (map (parens . prettyType) subtypes)
prettyTYpe (HSTypeVar var)
  = text var



prettyDeclaration (HSDecl name pattern expr)
  = sep [sep (signature ++ [char '=']), nest 2 (prettyExpr expr)]
 where
   signature = text name : map prettyPat pattern

prettyPat (PVar varname) = text varname

prettyExpr (HSApp operator operand) = sep [prettyExpr operator, nest 2 (prettyExpr operand)]
prettyExpr (HSVar varname) = text varname
prettyExpr (HSString cont) = text . show $ cont
prettyExpr (HSInt num)     = int num
prettyExpr (HSOp operator) = parens (text operator)



type Compilation = Either CompileError
newtype CompileError = CompileError [(String,IExpr)]

instance Show CompileError where
  show (CompileError errors)
    = unwords . intersperse "or" . map showErr $ errors
   where
     showErr (who, what) = "invalid " ++ who ++ " (" ++ show what ++ ")"


compileError :: String -> IExpr -> Compilation a
compileError who what = Left $ CompileError [(who,what)]

(CompileError err1) `compileErrorPlus` (CompileError err2) = CompileError (err1 ++ err2)

(left `orTry` right) expr = case left expr of
  Right leftsuccess -> return leftsuccess
  Left lefterr -> case right expr of
    Left righterr -> Left $ lefterr `compileErrorPlus` righterr
    Right rightsuccess -> return rightsuccess


compileModule :: IExpr -> Compilation HSMod
compileModule (ITreeSep "" (IGroup [IAtom "hasmod"]) body)
  = liftM2 HSMod (mapM compileImport . filter importStatement $ body)
                 (mapM compileDefinition . filter (not . importStatement) $ body)
 where
   importStatement (ITreeSep "" (IGroup [IAtom "import"]) _) = True
   importStatement _ = False

   compileDefinition = compileFuncDefinition `orTry` compileDataDefinition
compileModule mod = compileError "module" mod

compileImport :: IExpr -> Compilation HSImport
compileImport (ITreeSep "" (IGroup [IAtom "import"]) [IGroup mod])
  = HSImport `liftM` mapM unpack mod
 where
   unpack (IAtom m) = return m
   unpack modpart   = compileError "module part" modpart
compileImport imp = compileError "import" imp

compileFuncDefinition :: IExpr -> Compilation HSDef
compileFuncDefinition (ITreeSep "" (IGroup [IAtom "define"]) equations)
  = HSFuncDef `liftM` mapM compileDefEquation equations
compileFuncDefinition funcdef = compileError "function definition" funcdef

compileDataDefinition :: IExpr -> Compilation HSDef
compileDataDefinition (ITreeSep "" (IGroup [IAtom "define-data"])
                        [ITreeSep "=" (IGroup (IAtom typename : typeargs))
                          constructors])
  = HSDataDef typename (map unpack typeargs) `liftM` mapM compileDataConstructor constructors
 where
   unpack (IAtom v) = v
compileDataDefinition datadef = compileError "data definition" datadef

compileDataConstructor :: IExpr -> Compilation HSDataConstr
compileDataConstructor (IGroup (IAtom constrName : subtypes))
  = HSDataConstr constrName `liftM` mapM compileSubtype subtypes
 where
   compileSubtype (IList Parens (IAtom typename : typeargs))
     = HSType typename `liftM` mapM compileSubtype typeargs
   compileSubType (IAtom var) = return $ HSTypeVar var
   compileSubType ty = compileError "type" ty
compileDataConstructor dataconstr = compileError "data constructor" dataconstr

compileDefEquation :: IExpr -> Compilation HSDecl
compileDefEquation (ITreeSep "=" (IGroup (IAtom functionName : args)) [expr])
  = liftM2 (HSDecl functionName) (mapM compilePattern args) (compileExpr expr)
compileDefEquation equation = compileError "equation" equation

compilePattern :: IExpr -> Compilation HSPat
compilePattern (IAtom var) = return $ PVar var
compilePattern pat = compileError "pattern" pat

compileExpr :: IExpr -> Compilation HSExpr
compileExpr (IGroup (f : args)) = liftM2 multiApp (compileExpr f) (mapM compileExpr args)
compileExpr (IList Parens (f : args)) = liftM2 multiApp (compileExpr f) (mapM compileExpr args)
compileExpr (IAtom varname) = return $ HSVar varname
compileExpr (IString cont) = return $ HSString cont
compileExpr expr = compileError "expression" expr

multiApp :: HSExpr -> [HSExpr] -> HSExpr
multiApp = foldl HSApp



testMatch matcher = tolerantRead $ \input -> case matcher input of
  Nothing -> putStrLn "No Match"
  Just x  -> putStrLn "Match:" >> print (pretty x)
  

compileFile filename
  = tolerantParseFile filename $
      compileModule >>> eitherFailOr writeToFile
 where
   filebase = takeBaseName filename
   writeToFile = writeFile (filebase ++ ".hs") . show . prettyMod filebase

compileRead = tolerantRead $ compileModule >>> eitherFailOr (print . prettyMod "Main")


eitherFailOr f (Left err) = print err
eitherFailOr f (Right success) = f success

(f >>> g) x = g (f x)
