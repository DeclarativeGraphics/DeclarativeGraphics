module Backends.Haskell.Syntax where

import Pretty

import Data.List (intersperse,sortBy)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Text.PrettyPrint


type Name = String
type Var = String

-- module ... where <import>* <definition>*
data HSMod = HSMod [HSImport] [HSDef] deriving Show

-- <import>: import module.name
data HSImport = HSImport [String] deriving Show

-- <definition>: <funcdecl>* | data <name> <var>* = <constructor>*
data HSDef = HSFuncDef [HSDecl]
           | HSDataDef Name [Var] [HSDataConstr] deriving Show

-- <constructor>: <name> <type>*
data HSDataConstr = HSDataConstr Name [HSType] deriving Show

-- <type>: <name> <type>* | <var>
data HSType = HSType Name [HSType] | HSTypeVar Var deriving Show

-- <funcdecl>: <name> <pat>* = <expr>
data HSDecl = HSDecl Name [HSPat] HSExpr deriving Show

-- <pat>: <var>
data HSPat = PVar Var deriving Show

-- <expr>: <expr> <expr> | <var> | <string> | <int> | <name>
data HSExpr = HSExpr `HSApp` HSExpr | HSVar Var | HSString String | HSInt Int
            | HSOp Name deriving Show




-------- PRETTY PRINTING ---------

instance Pretty HSMod        where pretty = prettyMod "Main"
instance Pretty HSImport     where pretty = prettyImport
instance Pretty HSDef        where pretty = prettyDefinition
instance Pretty HSDataConstr where pretty = prettyDataConstr
instance Pretty HSType       where pretty = prettyType
instance Pretty HSDecl       where pretty = prettyDeclaration
instance Pretty HSPat        where pretty = prettyPat
instance Pretty HSExpr       where pretty = prettyExpr


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
  = text "data" <+> text typename <+> hsep (map text typeargs) <+>
    if null constructors
    then empty
    else sep (text "=" <+> prettyDataConstr (head constructors)
             :map ((text "|" <+>) . prettyDataConstr) (tail constructors))

prettyDataConstr (HSDataConstr name subtypes)
  = text name <+> sep (map prettyType subtypes)

prettyType (HSType typename subtypes)
  = parens $ text typename <+> sep (map prettyType subtypes)
prettyType (HSTypeVar var)
  = text var



prettyDeclaration (HSDecl name pattern expr)
  = sep [sep (signature ++ [char '=']), nest 2 (prettyExpr expr)]
 where
   signature = text name : map prettyPat pattern

prettyPat (PVar varname) = text varname

prettyExpr (HSApp operator operand) = parens $ sep [prettyExpr operator
                                                   ,nest 2 (prettyExpr operand)]
prettyExpr (HSVar varname) = hsVar varname
prettyExpr (HSString cont) = text . show $ cont
prettyExpr (HSInt num)     = int num
prettyExpr (HSOp operator) = parens (text operator)


hsVar varname = if hsOperator varname
                then parens . text $ varname
                else text . hsEscape $ varname

hsOperator = all operatorChar
 where
   operatorChar = (`elem` ":.,$<>+-*/%&^=?!~|#")

hsEscape = concatMap escapeChar
 where
   escapeChar c = fromMaybe [c] $ lookup c escapetable
   escapetable = [('-', "_minus_")
                 ,('+', "_plus_")
                 ,('*', "_star_")
                 ,('/', "_slash_")
                 ,('%', "_percent_")
                 ,('&', "_ampersand_")
                 ,('^', "_caret_")
                 ,('=', "_equals_")
                 ,('?', "_quest_")
                 ,('!', "_bang_")
                 ,('~', "_tilde_")
                 ,('|', "_bar_")
                 ,('#', "_hash_")
                 --,('_', "_underscore_")
                 ]
