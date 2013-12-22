module IExpr where

import Pretty

import Text.PrettyPrint


data IExpr s = IAtom String s
             | IString String s
             | IList [IExpr s]
             | IBraceList [IExpr s]
             | IBracketList [IExpr s]
             | IDecor (IExpr s) (IExpr s)
             | ITree (IExpr s) [IExpr s]
             | ITreeSep (String, s) (IExpr s) [IExpr s]
             | IHash (IExpr s) [String]
             deriving (Show)

data ParenType = Parens | Brackets | Braces deriving (Show, Eq)


instance Pretty (IExpr s) where
  pretty (IAtom cont _) = text cont
  pretty (IString cont _) = text $ show cont
  pretty (IList exprs) = text "List" <> parens (hsep $ map pretty exprs)
  pretty (IBraceList exprs)
    = text "List" <> (braces . sep $ map pretty exprs)
  pretty (IBracketList exprs)
    = text "List" <> (brackets . sep $ map pretty exprs)
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
getPositions (IAtom _ pos)                = [pos]
getPositions (IString _ pos)              = [pos]
getPositions (IList elems)                = concatMap getPositions elems
getPositions (IBraceList elems)           = concatMap getPositions elems
getPositions (IBracketList elems)         = concatMap getPositions elems
getPositions (IDecor decorator decorated) = getPositions decorator ++ getPositions decorated
getPositions (ITree root leaves)          = getPositions root ++ concatMap getPositions leaves
getPositions (ITreeSep (_, seperatorpos) root leaves)
                                          = getPositions root ++ [seperatorpos] ++ concatMap getPositions leaves
getPositions (IHash root contents)        = getPositions root

getPosition :: IExpr pos -> Maybe pos
getPosition expr = case getPositions expr of
  firstpos:rest -> Just firstpos
  otherwise     -> Nothing
