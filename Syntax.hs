module Syntax where

import Pretty

import Control.Applicative ((<*),(*>))
import Control.Monad (liftM, liftM2, guard)

import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)

import Text.Parsec
import Text.Parsec.String

import Text.PrettyPrint hiding (char)


------------- IExpr Data -------------

data IExpr = ISymbol String
           | IString String
           | IList ParenType [IExpr]
           | IGroup [IExpr]
           | IDecor IExpr IExpr
           | ITree IExpr [IExpr]
           | ITreeSep String IExpr [IExpr]
           | IHash IExpr [String]
           deriving (Show)

data ParenType = Parens | Brackets | Braces deriving (Show, Eq)


instance Pretty IExpr where
  pretty (ISymbol cont) = text cont
  pretty (IString cont) = text $ show cont
  pretty (IList parentype exprs)
    = text "Parens" <> (wrapInParens parentype . sep $ map pretty exprs)
   where wrapInParens Parens = parens
         wrapInParens Brackets = brackets
         wrapInParens Braces = braces
  pretty (IGroup exprs) = text "Group" <> parens (hsep $ map pretty exprs)
  pretty (IDecor decorator decorated)
    = text "Decor" <+> pretty decorator $+$
      nest 2 (pretty decorated)
  pretty (ITree rootexpr subexpr)
    = text "Tree" <+> pretty rootexpr $+$
      nest 2 (vcat $ map pretty subexpr)
  pretty (ITreeSep sep left right)
    = text "TreeSep" <+> parens (pretty left) <> text (':' : sep) $+$
      nest 2 (vcat $ map pretty right)
  pretty (IHash expr substrings)
    = text "Hash" <+> parens (pretty expr) $+$
      nest 2 (vcat $ map (prepend "|" . text) substrings)
   where
     prepend str = (text str <>)
     postpend str = (<> text str)
     surround str = prepend str . postpend str




------------ PARSER STUFF --------------

type ParserExtension a = a -> Parser a


extend :: ParserExtension a -> (Parser a, a) -> Parser a
parser `extend` (embedded, empty) = withEmbedded <|> parser empty
 where
   withEmbedded = do
     embeddedExpr <- embedded
     maybeExtension <- parseMaybe $ parser embeddedExpr
     return $ fromMaybe embeddedExpr maybeExtension
   parseMaybe parser = liftM Just parser <|> return Nothing


extOr :: ParserExtension a -> ParserExtension a -> ParserExtension a
(extension1 `extOr` extension2) root = extension1 root <|> extension2 root




------------ IExpr Parser ------------

iexpr :: Parser IExpr
iexpr = decorator (list atomic <|> onlyTreeIExpr) iexpr <|> treeIExpr iexpr
 where
   onlyTreeIExpr = treeIExpr onlyTreeIExpr


treeIExpr :: Parser IExpr -> Parser IExpr
treeIExpr leaf = tree leaf $ (doublepoint leaf `extOr` hash) `extend` (group atomic, IGroup [])


delimiters = " \t\r\n(){}[]:#@,|\""


---- Atomic Expressions

atomic :: Parser IExpr
atomic = symbol <|> stringexpr <|> list atomic

symbol :: Parser IExpr
symbol = liftM ISymbol $ many1 $ noneOf delimiters

stringexpr :: Parser IExpr
stringexpr = IString `liftM` (char '"' *> escapedLiteralParser '\\' escape "\"" <* char '"')
 where
   escape c = fromMaybe c $ lookup c escapeTable
   escapeTable = zip "trn" "\t\r\n"


---- Structuring Expressions


-- <elem> ...
group :: Parser IExpr -> Parser IExpr
group elem = liftM IGroup $ many1 paddedElem
 where
   paddedElem = horizspaces *> elem <* horizspaces 


-- (<atomexpr> ...) | [<atomexpr> ...] | {<atomexpr> ...}
list :: Parser IExpr -> Parser IExpr
list elem = choice $ map parenparser parentable
 where
   subexpr = many $ spaces *> elem <* spaces

   parenparser ([open,close], constructor)
     = liftM (IList constructor) $ between (char open) (char close) subexpr

   parentable = [("()", Parens)
                ,("[]", Brackets)
                ,("{}", Braces)
                ]

-- <embedded>
--   <leaf>
--   ...
tree :: Parser IExpr -> Parser IExpr -> Parser IExpr
tree leaf embedded = do
  spaces
  indent <- getColumn
  rootexpr <- embedded
  spaces
  column <- getColumn
  if column > indent
    then liftM (ITree rootexpr) (leaves leaf column)
    else return rootexpr


-- `root`:
--   <leaf>
--   ...
doublepoint :: Parser IExpr -> ParserExtension IExpr
doublepoint leaf root = do
  char ':'
  seperator <- many $ noneOf delimiters
  horizspaces
  liftM (ITreeSep seperator root) $ spaces >> subexprs
 where
   subexprs = leaves leaf =<< getColumn


-- @<decorator> <decorated>   -- may need to be seperated by newline if decorator is tree
decorator :: Parser IExpr -> Parser IExpr -> Parser IExpr
decorator decorator decorated = do
  char '@'
  decoratorexpr <- decorator
  decoratedexpr <- decorated
  return $ IDecor decoratorexpr decoratedexpr


-- `root`#
--   <sublines>
--   <on same>
--   <indentation>
hash :: ParserExtension IExpr
hash root = do
  char '#'
  horizspaces
  liftM (IHash root) $ spaces >> getColumn >>= sublines
 where
   parseLine = many (noneOf "\n") <* newline
   parseMultipleLines indent
     = do column <- getColumn
          if column < indent
            then option [] $     (char ' ' >> parseMultipleLines indent)
                             <|> (newline  >> liftM ("":) (parseMultipleLines indent))
            else liftM2 (:) parseLine (parseMultipleLines indent)
   sublines indent = dropWhileEnd null `liftM` parseMultipleLines indent




-------- Parser Helpers --------

escapedLiteralParser escChar escape delimiters
  = many $ (char escChar *> escapedChar) <|> noneOf delimiters
 where
   escapedChar = liftM escape anyChar


leaves :: Parser IExpr -> Column -> Parser [IExpr]
leaves embedded indent = many $ leaf <* spaces
 where
   leaf = do
     column <- getColumn
     guard $ column >= indent
     embedded


horizspaces :: Parser ()
horizspaces = skipMany  $ oneOf " \t"


getColumn :: Parser Column
getColumn = liftM sourceColumn getPosition
