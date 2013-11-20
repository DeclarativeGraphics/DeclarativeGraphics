module IExpr where

import Pretty

import Text.PrettyPrint


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
