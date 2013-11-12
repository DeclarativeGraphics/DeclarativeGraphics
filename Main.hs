module Main where

import Control.Applicative ((<*),(*>))
import Control.Monad (liftM, liftM2, guard)

import Data.List (intersperse,dropWhileEnd)
import Data.Maybe (fromMaybe)

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator

import Text.PrettyPrint hiding (char)


data IExpr = IAtom String
           | IString String
           | IList ParenType [IExpr]
           | IGroup [IExpr]
           | IDecor IExpr IExpr
           | ITree IExpr [IExpr]
           | ITreeSep String IExpr [IExpr]
           | IHash IExpr [String]
           deriving (Show)

data ParenType = Parens | Brackets | Braces deriving (Show)


atom :: Parser IExpr
atom = liftM IAtom $ many1 $ noneOf delimiters

stringexpr :: Parser IExpr
stringexpr = do
  char '"'
  content <- many $ (char '\\' *> escapedChar) <|> noneOf "\""
  char '"'
  return $ IString content
 where
   escapedChar = liftM escape anyChar
   escape c = fromMaybe c $ lookup c escapeTable
   escapeTable = zip "trn" "\t\r\n"

delimiters = " \t\r\n(){}[]:#@,|\""

list :: Parser IExpr
list = choice $ map parenparser parendesc
 where
   subexpr = many $ spaces *> atomexpr <* spaces

   parenparser ([open,close], constructor)
     = liftM (IList constructor) $ between (char open) (char close) subexpr

   parendesc = [("()", Parens)
               ,("[]", Brackets)
               ,("{}", Braces)
               ]


atomexpr :: Parser IExpr
atomexpr = atom <|> stringexpr <|> list

line :: Parser IExpr
line = do
  firstgroup <- many paddedexpr
  choice $ map ($ IGroup firstgroup) [endofline, doublepoint, hash]
 where
   paddedexpr = horizspaces *> atomexpr <* horizspaces 

   endofline group = newline >> return group

   hash root = do
     char '#'
     horizspaces
     liftM (IHash root) $ spaces >> getColumn >>= substrings
    where
      parseLine = many (noneOf "\n") <* newline
      substrings indent
        = do column <- getColumn
             if column < indent
             then option [] $ (char ' ' >> substrings indent)
                              <|> (newline >> liftM ("":) (substrings indent))
             else liftM2 (:) parseLine (substrings indent)
      sublines indent = dropWhileEnd null `liftM` substrings indent

   doublepoint root = do
     char ':'
     seperator <- many $ noneOf delimiters
     horizspaces
     liftM (ITreeSep seperator root) $ spaces >> subexprs
    where
      subexprs = leaves =<< getColumn

decorator :: Parser IExpr
decorator = do
  char '@'
  decoratorexpr <- tree
  decoratedexpr <- iexpr
  return $ IDecor decoratorexpr decoratedexpr


tree :: Parser IExpr
tree = do
  spaces
  indent <- getColumn
  rootexpr <- line
  spaces
  column <- getColumn
  if column > indent
    then liftM (ITree rootexpr) (leaves column)
    else return rootexpr

iexpr :: Parser IExpr
iexpr = decorator <|> tree

leaves :: Column -> Parser [IExpr]
leaves indent = many $ leaf <* horizspaces
 where
   leaf = do
     column <- getColumn
     guard $ column >= indent
     iexpr



horizspaces, horizspaces1 :: Parser ()
horizspaces  = skipMany  $ oneOf " \t"
horizspaces1 = skipMany1 $ oneOf " \t"

getColumn :: Parser Column
getColumn = liftM sourceColumn getPosition



pretty :: IExpr -> Doc
pretty (IAtom cont) = text cont
pretty (IString cont) = text $ show cont
pretty (IList parentype exprs)
  = text "Parens" <> (wrapInParens parentype $ sep $ map pretty exprs)
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
    nest 2 (vcat $ map text substrings)





-- HELPER
--

fromRight (Right x) = x


testParse = do
  parsed <- parseFromFile (many1 iexpr) "test.syn"
  case parsed of
    Left err -> print err
    Right exprs -> mapM_ putStrLn $ intersperse "" $ map (show . pretty) $ exprs
