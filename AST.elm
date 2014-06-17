module AST where

import Tree (..)
import Bounded (..)
import DrawingUtils (..)

----- AST -----
data Atom = Num Int | Var String

app = Node
num = leaf . Num
var = leaf . Var


fatLine = { defaultLine | width <- 10, cap <- Round, join <- Smooth }

renderAtom : (String -> a) -> Atom -> a
renderAtom renderString atom = case atom of
  Num i    -> renderString (show i)
  Var name -> renderString name

renderFocusedAtom color = renderAtom (drawNode { fatLine | color <- color } . btext)
renderStandardAtom = renderAtom (drawNode defaultLine . btext)

renderFocusedTree renderFocus (Node focused children)
  = renderFocus focused `connectChildren` map renderStandardTree children
renderStandardTree = foldTree renderStandardAtom connectChildren



renderExprInFocus : (Atom -> Bounded Form) -> TreeFocus Atom -> Bounded Form
renderExprInFocus renderFocus focus = head <| foldZipper (renderFocusedTree renderFocus)
                                                         renderStandardAtom
                                                         connectChildren
                                                         focus
