module IExprParser where

import IExpr hiding (getPosition)

import Control.Applicative ((<*),(*>))
import Control.Monad (liftM, liftM2, guard, void)

import Data.List (dropWhileEnd)
import Data.Maybe (fromMaybe)

import Text.Parsec
import Text.Parsec.String



------------ PARSER STUFF --------------

type ParserExtension a = a -> Parser a


extend :: ParserExtension a -> Parser a -> Parser a
parser `extend` embedded = do
  embeddedExpr <- embedded
  maybeExtension <- optionMaybe $ parser embeddedExpr
  return $ fromMaybe embeddedExpr maybeExtension


extendEmpty :: ParserExtension a -> (Parser a, a) -> Parser a
parser `extendEmpty` (embedded, empty) = (parser `extend` embedded) <|> parser empty


extOr :: ParserExtension a -> ParserExtension a -> ParserExtension a
(extension1 `extOr` extension2) root = extension1 root <|> extension2 root


withPosition parser = do pos <- getPosition; exprBuilder <- parser ; return (exprBuilder pos)


------------ IExpr Parser ------------

type IExprSource = IExpr SourcePos

iexpr :: Parser IExprSource
iexpr = decorator (atomic <|> onlyTreeIExpr) iexpr <|> treeIExpr iexpr
 where
   onlyTreeIExpr = treeIExpr onlyTreeIExpr


treeIExpr :: Parser IExprSource -> Parser IExprSource
treeIExpr leaf = tree leaf $ specialTree `extendEmpty` (list delimitedExpr, IList [])
 where
   delimitedExpr = inlineDecorator atomic `extend` atomic
   specialTree = doublepoint leaf `extOr` hash


delimiters = " \t\r\n(){}[]:#@|\"%"


---- Delimited Expressions

atomic :: Parser IExprSource
atomic = atom <|> stringexpr <|>
         inParens "()" iexprOrEmpty <|> braceList iexpr <|> bracketList iexpr
 where
   iexprOrEmpty = iexpr <|> return (IList [])

-- nonspaceletters...
atom :: Parser IExprSource
atom = withPosition $ liftM IAtom $ many1 $ noneOf delimiters

-- "letters..."
stringexpr :: Parser IExprSource
stringexpr = withPosition $
               IString `liftM` (between (char '"') (char '"') $
                                  escapedLiteralParser '\\' escape "\"")
 where
   escape c = fromMaybe c $ lookup c escapeTable
   escapeTable = zip "trn" "\t\r\n"


-- `decorator`@<elem>
inlineDecorator :: Parser IExprSource -> ParserExtension IExprSource
inlineDecorator elem decorator = do
  char '@'
  decorated <- elem
  return $ IDecor decorator decorated


-- '<open>' <elem> '<close>'
inParens :: [Char] -> Parser IExprSource -> Parser IExprSource
inParens [open, close] elem = between (char open) (char close) $
                                ignorespaces *> elem <* ignorespaces


-- {<elem> ...}
braceList :: Parser IExprSource -> Parser IExprSource
braceList elem = inParens "{}" $ liftM IBraceList (seperatedList elem)

-- [<elem> ...]
bracketList :: Parser IExprSource -> Parser IExprSource
bracketList elem = inParens "[]" $ liftM IBracketList (seperatedList elem)



---- Structuring Expressions

-- <elem> ...
list :: Parser IExprSource -> Parser IExprSource
list elem = liftM IList $ many1 paddedElem
 where
   paddedElem = horizspaces *> elem <* horizspaces 


-- <elem> ... | <elem> '|' ...
seperatedList :: Parser IExprSource -> Parser [IExprSource]
seperatedList elem = many $ ignorespaces *> elem <* ignorespaces <* option () elemDelimiter
 where
   elemDelimiter = void (char '|')


-- <embedded>
--   <leaf>
--   ...
tree :: Parser IExprSource -> Parser IExprSource -> Parser IExprSource
tree leaf embedded = do
  ignorespaces
  indent <- getColumn
  rootexpr <- embedded
  ignorespaces
  column <- getColumn
  if column > indent
    then do subexprs <- leaves leaf column
            case subexprs of
              [] -> return rootexpr
              _  -> return $ ITree rootexpr subexprs
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
  liftM (ITreeSep seperatorWithPos root) $ ignorespaces >> subexprs
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
  ignorespaces
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



-------- Comments --------

-- % comment until end of line
comment :: Parser ()
comment = do
  char '%'
  skipMany (noneOf "\n")
  newline
  return ()

-- any whitespace or a comment
ignorespace :: Parser ()
ignorespace = void space <|> comment

-- many of ignorespace
ignorespaces :: Parser ()
ignorespaces = skipMany ignorespace


-------- Parser Helpers --------

escapedLiteralParser escChar escape delimiters
  = many $ (char escChar *> escapedChar) <|> noneOf delimiters
 where
   escapedChar = liftM escape anyChar


leaves :: Parser IExprSource -> Column -> Parser [IExprSource]
leaves embedded indent = many $ leaf <* ignorespaces
 where
   leaf = do
     column <- getColumn
     guard $ column >= indent
     embedded


horizspaces :: Parser ()
horizspaces = skipMany $ oneOf " \t"


getColumn :: Parser Column
getColumn = liftM sourceColumn getPosition
