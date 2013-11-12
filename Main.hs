module Main where

import Control.Applicative ((<*),(*>))
import Control.Monad (liftM, guard)

import Data.List

import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Combinator

import Text.PrettyPrint hiding (char)


data IExpr = IAtom String
           | IList ParenType [IExpr]
           | IGroup [IExpr]
           | ITree IExpr [IExpr]
           | IDouble String IExpr [IExpr]
           deriving (Show)

data ParenType = Parens | Brackets | Braces deriving (Show)


atom :: Parser IExpr
atom = liftM IAtom $ many1 $ noneOf delimiters

delimiters = " \t\r\n(){}[]:,|\""

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
atomexpr = atom <|> list

line :: Parser IExpr
line = do
  firstgroup <- many paddedexpr
  choice $ map ($ IGroup firstgroup) [endofline, doublepoint]
 where
   paddedexpr = horizspaces *> atomexpr <* horizspaces 
   endofline group = newline >> return group
   doublepoint root = do
     char ':'
     seperator <- many $ noneOf delimiters
     horizspaces
     liftM (IDouble seperator root) $ forcedIndent <|> restline
    where
      forcedIndent = do
        newline
        spaces
        leaves =<< getColumn
      restline = leaves =<< getColumn

        

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

leaves :: Column -> Parser [IExpr]
leaves indent = many $ leaf <* horizspaces
 where
   leaf = do
     column <- getColumn
     guard $ column >= indent
     tree



horizspaces, horizspaces1 :: Parser ()
horizspaces  = skipMany  $ oneOf " \t"
horizspaces1 = skipMany1 $ oneOf " \t"

getColumn :: Parser Column
getColumn = liftM sourceColumn getPosition



pretty :: IExpr -> Doc
pretty (IAtom cont) = text cont
pretty (IList parentype exprs)
  = text "Parens" <> (wrapInParens parentype $ sep $ map pretty exprs)
 where wrapInParens Parens = parens
       wrapInParens Brackets = brackets
       wrapInParens Braces = braces
pretty (IGroup exprs) = text "Group" <> parens (hsep $ map pretty exprs)
pretty (ITree rootexpr subexpr)
  = text "Tree" <+> pretty rootexpr $+$ nest 2 (vcat $ map pretty subexpr)
pretty (IDouble sep left right)
  = text "Double" <+> parens (pretty left) <> text (':' : sep) $+$
    nest 2 (vcat $ map pretty right)





-- HELPER
--

fromRight (Right x) = x


testParse = do
  parsed <- parseFromFile (many1 tree) "test.syn"
  case parsed of
    Left err -> print err
    Right exprs -> mapM_ putStrLn $ intersperse "" $ map (show . pretty) $ exprs
