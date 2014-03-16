module Main where

import Language.Simple
import Diagrams.Prelude hiding ((~~))
import Diagrams.Backend.Cairo
import Graphics.FRP
import Graphics.GtkFRP
import Graphics.GtkFRPMain
import Graphics.DiagramsFRP
import qualified Graphics.UI.Gtk as Gtk


data EditorState = EditorState {
  ast :: SimpleExpr
} deriving (Show)


data Hole = Hole

frpsys :: FRP (Event GtkEvent) (Diagram Cairo R2)
frpsys = drawing <~ mousePos ~~ editorState
  where drawing mouseXY ast = moveTo (p2 mouseXY)
                            $ scaleY (-1)
                            $ text' (interpret ast) # scale 20
        editorState = foldp editAST (SEConst (SVNum 42)) <<< keyPress
          where editAST key ast = SEApp (SEApp (SEVar "+") ast)
                                               (SEConst (SVNum 1))

text' :: Show a => a -> Diagram Cairo R2
text' = text . show

main = runGtkFRP diagramsFRPWidget frpsys
