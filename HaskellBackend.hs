module HaskellBackend where

import Syntax

import Control.Applicative (Applicative(..),liftA,liftA2,liftA3)
import Control.Monad (guard,liftM,liftM2,mplus,zipWithM)
import Data.Either (partitionEithers)
import Data.List (intersperse,sortBy)
import Data.Maybe (catMaybes,fromMaybe)
import Data.Ord (comparing)
import Text.PrettyPrint
import System.FilePath (takeBaseName)


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




------------- GENERAL COMPILATION ----------------


data Compilation a b = CompileError [ErrorContext a] | CompileResult b
data ErrorContext a = ErrorMessage String
                    | ErrorSource String a
                    | ErrorContext String [ErrorContext a]
                    deriving Show

instance Functor (Compilation a) where
  fmap f (CompileResult result) = CompileResult (f result)
  fmap f (CompileError error)   = CompileError error

instance Applicative (Compilation a) where
  pure = CompileResult

  (CompileResult f)      <*> (CompileResult arg)    = CompileResult (f arg)
  (CompileError errors1) <*> (CompileError errors2) = CompileError (errors1 ++ errors2)
  (CompileError errors)  <*> nonerror               = CompileError errors
  nonerror               <*> (CompileError errors)  = CompileError errors


getCompilation (CompileError err) = Left err
getCompilation (CompileResult res) = Right res


compileError :: String -> a -> Compilation a b
compileError what source = CompileError [ErrorSource what source]

simpleError :: String -> Compilation a b
simpleError message = CompileError [ErrorMessage message]

errs1 `compileErrorPlus` errs2 = errs1 ++ errs2

(left `orTry` right) expr = case left expr of
  CompileError lefterr    -> case right expr of
    CompileError righterr -> CompileError $ lefterr ++ righterr
    rightsuccess          -> rightsuccess
  leftsuccess             -> leftsuccess

getBoth = liftA2 (,)

lmany melem [] = pure []
lmany melem (x:xs) = liftA2 (:) (melem x) (lmany melem xs)

lmany1 msub = liftA (uncurry (:)) . (msub `lcons` lmany msub)

llist []     []     = pure []
llist (m:ms) (x:xs) = liftA2 (:) (m x) (llist ms xs)
llist []     _      = simpleError "too many elements"
llist _      []     = simpleError "too few elements"

lcons mhead mtail (x:xs) = liftA2 (,) (mhead x) (mtail xs)
lcons _     _     []     = simpleError "expected non-empty list"

withContext context (CompileResult result) = CompileResult result
withContext context (CompileError error)   = CompileError [ErrorContext context error]



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




--------------- HASMOD Compilation --------------

iModule = liftA (uncurry HSMod . partitionEithers . snd) . iModuleExpr
  where
    iModuleExpr = mtreesep' (msimplegroup ["hasmod"]) ":"
                            (lmany iModuleBodyStatement)
    iModuleBodyStatement = (liftA Left . iImport) `orTry` (liftA Right . iDefinition)

iImport = liftA hsImport . mImport
 where
   mImport = mtreesep' (msimplegroup ["import"]) ":" (llist [mgroup (lmany (matom mAny))])
   hsImport (_,[moduleparts]) = HSImport moduleparts

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




compileModule filename
  = tolerantParseFile filename $
      iModule >>> getCompilation >>> eitherFailOr writeToFile
 where
   filebase = takeBaseName filename
   writeToFile = writeFile (filebase ++ ".hs") . show . prettyMod filebase

compileModulePrint filename
  = tolerantParseFile filename $
      iModule >>> getCompilation >>> eitherFailOr (print . prettyMod filebase)
 where
   filebase = takeBaseName filename

compileRead compiler = tolerantRead $ compiler >>> getCompilation >>> eitherFailOr prettyPrint

prettyPrint :: Pretty a => a -> IO ()
prettyPrint = print . pretty


eitherFailOr f (Left err) = print err
eitherFailOr f (Right success) = f success

(f >>> g) x = g (f x)
