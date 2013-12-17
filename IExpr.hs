module IExpr where

import Pretty

import Text.PrettyPrint


data IExpr s = ISymbol String s
             | IString String s
             | IGroup ParenType [IExpr s]
             | IList [IExpr s]
             | IDecor (IExpr s) (IExpr s)
             | ITree (IExpr s) [IExpr s]
             | ITreeSep (String, s) (IExpr s) [IExpr s]
             | IHash (IExpr s) [String]
             deriving (Show)

data ParenType = Parens | Brackets | Braces deriving (Show, Eq)


instance Pretty (IExpr s) where
  pretty (ISymbol cont _) = text cont
  pretty (IString cont _) = text $ show cont
  pretty (IGroup parentype exprs)
    = text "Group" <> (wrapInParens parentype . sep $ map pretty exprs)
   where wrapInParens Parens = parens
         wrapInParens Brackets = brackets
         wrapInParens Braces = braces
  pretty (IList exprs) = text "List" <> parens (hsep $ map pretty exprs)
  pretty (IDecor decorator decorated)
    = text "Decor" <+> pretty decorator $+$
      nest 2 (pretty decorated)
  pretty (ITree rootexpr subexpr)
    = text "Tree" <+> pretty rootexpr $+$
      nest 2 (vcat $ map pretty subexpr)
  pretty (ITreeSep (sep,_) left right)
    = text "TreeSep" <+> parens (pretty left) <> text (':' : sep) $+$
      nest 2 (vcat $ map pretty right)
  pretty (IHash expr substrings)
    = text "Hash" <+> parens (pretty expr) $+$
      nest 2 (vcat $ map (prepend "|" . text) substrings)
   where
     prepend str = (text str <>)
     postpend str = (<> text str)
     surround str = prepend str . postpend str



getPositions :: IExpr pos -> [pos]
getPositions (ISymbol _ pos)              = [pos]
getPositions (IString _ pos)              = [pos]
getPositions (IGroup _ elems)             = concatMap getPositions elems
getPositions (IList elems)                = concatMap getPositions elems
getPositions (IDecor decorator decorated) = getPositions decorator ++ getPositions decorated
getPositions (ITree root leaves)          = getPositions root ++ concatMap getPositions leaves
getPositions (ITreeSep (_, seperatorpos) root leaves)
                                          = getPositions root ++ [seperatorpos] ++ concatMap getPositions leaves
getPositions (IHash root contents)        = getPositions root

getPosition :: IExpr pos -> Maybe pos
getPosition expr = case getPositions expr of
  firstpos:rest -> Just firstpos
  otherwise     -> Nothing
