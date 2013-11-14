module HaskellBackend where

import Syntax

import Control.Monad (guard,liftM,mplus,zipWithM)
import Data.List (intersperse,sortBy)
import Data.Maybe (catMaybes)
import Data.Ord (comparing)
import Text.PrettyPrint
import System.FilePath (takeBaseName)


data HSMod = HSMod [HSImport] [HSDef] deriving Show
data HSImport = HSImport [String] deriving Show
data HSDef = HSFuncDef [HSDecl]
           | HSDataDef String [String] [HSDataConstr] deriving Show
data HSDataConstr = HSDataConstr String [HSType] deriving Show
data HSType = HSType String [HSType] | HSTypeVar String deriving Show
data HSDecl = HSDecl String [HSPat] HSExpr deriving Show
data HSPat = PVar String deriving Show
data HSExpr = HSApp HSExpr HSExpr | HSVar String | HSString String | HSInt Int
            | HSOp String deriving Show

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
  = text "import" <+> (hcat $ punctuate (char '.') $ map text modparts)

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




compileModule :: IExpr -> HSMod
compileModule (ITreeSep "" (IGroup [IAtom "hasmod"]) body)
  = HSMod (map compileImport $ filter importStatement body)
          (mapMaybe compileDefinition . filter (not . importStatement) $ body)
 where
   importStatement (ITreeSep "" (IGroup [IAtom "import"]) _) = True
   importStatement _ = False

   compileDefinition expr = compileFuncDefinition expr `mplus` compileDataDefinition expr

compileImport (ITreeSep "" (IGroup [IAtom "import"]) [IGroup mod])
  = HSImport (map unpack mod)
 where
   unpack (IAtom m) = m

compileFuncDefinition :: IExpr -> Maybe HSDef
compileFuncDefinition (ITreeSep "" (IGroup [IAtom "define"]) equations)
  = Just $ HSFuncDef (map compileDefEquations equations)
compileFuncDefinition _ = Nothing

compileDataDefinition :: IExpr -> Maybe HSDef
compileDataDefinition (ITreeSep "" (IGroup [IAtom "define-data"])
                        [ITreeSep "=" (IGroup (IAtom typename : typeargs))
                          constructors])
  = Just $ HSDataDef typename (map unpack typeargs) (map compileDataConstructor constructors)
 where
   unpack (IAtom v) = v
compileDataDefinition _ = Nothing

compileDataConstructor :: IExpr -> HSDataConstr
compileDataConstructor (IGroup (IAtom constrName : subtypes))
  = HSDataConstr constrName (map compileSubtype subtypes)
 where
   compileSubtype (IList Parens (IAtom typename : typeargs))
     = HSType typename (map compileSubtype typeargs)
   compileSubType (IAtom var) = HSTypeVar var

compileDefEquations :: IExpr -> HSDecl
compileDefEquations (ITreeSep "=" (IGroup (IAtom functionName : args)) [expr])
  = HSDecl functionName (map compilePattern args) (compileExpr expr)

compilePattern :: IExpr -> HSPat
compilePattern (IAtom var) = PVar var

compileExpr :: IExpr -> HSExpr
compileExpr (IGroup (f : args)) = multiApp (compileExpr f) (map compileExpr args)
compileExpr (IAtom varname) = HSVar varname
compileExpr (IString cont) = HSString cont

multiApp :: HSExpr -> [HSExpr] -> HSExpr
multiApp = foldl HSApp

--definition = do
--  (_, definition) <- matchHierarchy1 (group (atom "define"))
--                                     matchDefinition
--  return definition

testMatch matcher = tolerantRead $ \input -> case matcher input of
  Nothing -> putStrLn "No Match"
  Just x  -> putStrLn "Match:" >> print (pretty x)
  

compileFile filename
  = tolerantParseFile filename $
      writeFile (filebase ++ ".hs") . show . prettyMod filebase . compileModule
 where
   filebase = takeBaseName filename
