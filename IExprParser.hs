module IExprParser where

import IExpr hiding (getPosition)

import Control.Applicative ((<*),(*>))
import Control.Monad (liftM, liftM2, guard)

import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)

import Text.Parsec
import Text.Parsec.String



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


withPosition parser = do pos <- getPosition; exprBuilder <- parser ; return (exprBuilder pos)


------------ IExpr Parser ------------

type IExprSource = IExpr SourcePos

iexpr :: Parser IExprSource
iexpr = decorator (group atomic <|> onlyTreeIExpr) iexpr <|> treeIExpr iexpr
 where
   onlyTreeIExpr = treeIExpr onlyTreeIExpr


treeIExpr :: Parser IExprSource -> Parser IExprSource
treeIExpr leaf = tree leaf $ (doublepoint leaf `extOr` hash) `extend` (list atomic, IList [])


delimiters = " \t\r\n(){}[]:#@,|\""


---- Atomic Expressions

atomic :: Parser IExprSource
atomic = symbol <|> stringexpr <|> group atomic

symbol :: Parser IExprSource
symbol = withPosition $ liftM ISymbol $ many1 $ noneOf delimiters

stringexpr :: Parser IExprSource
stringexpr = withPosition $ IString `liftM` (between (char '"') (char '"') $ escapedLiteralParser '\\' escape "\"")
 where
   escape c = fromMaybe c $ lookup c escapeTable
   escapeTable = zip "trn" "\t\r\n"


---- Structuring Expressions


-- <elem> ...
list :: Parser IExprSource -> Parser IExprSource
list elem = liftM IList $ many1 paddedElem
 where
   paddedElem = horizspaces *> elem <* horizspaces 


-- (<atomexpr> ...) | [<atomexpr> ...] | {<atomexpr> ...}
group :: Parser IExprSource -> Parser IExprSource
group elem = choice $ map parenparser parentable
 where
   subexpr = many $ spaces *> elem <* spaces

   parenparser ([open,close], parentype)
     = liftM (IGroup parentype) $ between (char open) (char close) subexpr

   parentable = [("()", Parens)
                ,("[]", Brackets)
                ,("{}", Braces)
                ]

-- <embedded>
--   <leaf>
--   ...
tree :: Parser IExprSource -> Parser IExprSource -> Parser IExprSource
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
doublepoint :: Parser IExprSource -> ParserExtension IExprSource
doublepoint leaf root = do
  pos <- getPosition
  char ':'
  seperator <- many $ noneOf delimiters
  horizspaces
  let seperatorWithPos = (seperator, pos)
  liftM (ITreeSep seperatorWithPos root) $ spaces >> subexprs
 where
   subexprs = leaves leaf =<< getColumn


-- @<decorator> <decorated>   -- may need to be seperated by newline if decorator is tree
decorator :: Parser IExprSource -> Parser IExprSource -> Parser IExprSource
decorator decorator decorated = do
  char '@'
  decoratorexpr <- decorator
  decoratedexpr <- decorated
  return $ IDecor decoratorexpr decoratedexpr


-- `root`#
--   <sublines>
--   <on same>
--   <indentation>
hash :: ParserExtension IExprSource
hash root = do
  char '#'
  spaces
  liftM (IHash root) $ getColumn >>= sublines
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


leaves :: Parser IExprSource -> Column -> Parser [IExprSource]
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
