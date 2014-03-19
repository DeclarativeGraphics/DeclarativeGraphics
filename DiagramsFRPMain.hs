module Main where

import Language.Simple
import Diagrams.Prelude hiding ((~~))
import Diagrams.Backend.Cairo
import Graphics.FRP
import Graphics.FRPUtils
import Graphics.GtkFRP
import Graphics.GtkFRPMain
import Graphics.WidgetFRP
import Graphics.WidgetFRPUtils
import Graphics.DiagramsFRP
import qualified Graphics.UI.Gtk as Gtk


data EditorState = EditorState {
  ast :: SimpleExpr
} deriving (Show)


data Hole = Hole

frpsys :: FRP (Event WidgetEvent) (Behavior (Diagram Cairo R2))
frpsys = drawing <~ (hold (0,0) <<< relMove <<< mouseMove) ~~ constant (200,200) {-movePosition (0,0) (drag MBLeft)-}
  where drawing txt mouseXY = moveTo (p2 mouseXY)
                            $ scaleY (-1)
                            $ text' txt # scale 20

        editorState = foldp editAST (SEConst (SVNum 42)) <<< keyPress
          where editAST key ast = SEApp (SEApp (SEVar "+") ast)
                                               (SEConst (SVNum 1))

text' :: Show a => a -> Diagram Cairo R2
text' = text . show

main = print frpsys >> runGtkFRP (frpWidget renderDiagram) frpsys
