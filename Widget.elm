module Widget where

import DrawingUtils (..)
import Tree (..)
import MaybeUtils (..)
import Bounded (..)

import AST (..)
import Input (..)
import TextInput (..)


example : Tree Atom
example = app (Var "foldl") [var "+"
                            ,var "0"
                            ,app (Var "map") [
                               app (Var "+") [
                                 app (Var "/") [var "widthLeft", var "2"],
                                 app (Var "/") [var "widthRight", var "2"]
                                 ],
                               var "neighbors"]
                            ]
exampleWithFocus = zipTree example


data Widget1 = Widget1 (Input -> Widget1) (Bounded Form)

normalModeWidget focus =
  let onTreeFocus f = normalModeWidget (f focus)
      changeToInsertMode () = case focus of
        TreeFocus _ _ (Node (Var focussedAtom) _) _
          -> insertModeWidget (toTextInput focussedAtom) focus

      interpretNormalModeKeyboardInput : Input -> Widget1
      interpretNormalModeKeyboardInput k = case k of
        Letter 'j'       -> onTreeFocus <| foldMaybe goDown
        Letter 'k'       -> onTreeFocus <| foldMaybe goUp
        Letter 'h'       -> onTreeFocus <| foldMaybe goLeft
        Letter 'l'       -> onTreeFocus <| foldMaybe goRight
        Special ArrDown  -> onTreeFocus <| foldMaybe goDown
        Special ArrUp    -> onTreeFocus <| foldMaybe goUp
        Special ArrLeft  -> onTreeFocus <| foldMaybe goLeft
        Special ArrRight -> onTreeFocus <| foldMaybe goRight

        Letter 'a' -> onTreeFocus <| modifyFocus (addChild (var "0"))
        Letter 'x' -> onTreeFocus <| foldMaybe deleteFocussed
        Letter 'H' -> onTreeFocus <| foldMaybe moveLeft
        Letter 'L' -> onTreeFocus <| foldMaybe moveRight

        Letter 'i' -> changeToInsertMode ()
        _          -> normalModeWidget focus

  in Widget1 interpretNormalModeKeyboardInput <| renderExprInFocus (renderFocussedAtom red) focus


insertModeWidget content focus =
  let returnFromInsertMode content focus =
        if isTextInputEmpty content
        then normalModeWidget focus
        else normalModeWidget (modifyFocus (setNodeValue (Var <| fromTextInput content)) focus)

      onInputContent : (TextInput -> TextInput) -> Widget1
      onInputContent f = insertModeWidget (f content) focus

      interpretInsertModeKeyboardInput : Input -> Widget1
      interpretInsertModeKeyboardInput k = case k of
        Special Escape    -> returnFromInsertMode content focus
        Special Backspace -> onInputContent <| foldMaybe textInputDelete
        Special ArrLeft   -> onInputContent <| foldMaybe textInputMoveLeft
        Special ArrRight  -> onInputContent <| foldMaybe textInputMoveRight
        Letter x          -> onInputContent <| textInputInsert x
        _                 -> insertModeWidget content focus

      renderInputField (prev,after) _ = 
        let drawChars = btext . String.fromList
            cursor    = bsegment (0,-9) (0,9) $> traced (solid black)
        in drawNode { fatLine | color <- blue }
             <| besides [drawChars (reverse prev), cursor, drawChars after]

  in Widget1 interpretInsertModeKeyboardInput <| renderExprInFocus (renderInputField content) focus


drawWidget : Widget1 -> Bounded Form
drawWidget (Widget1 _ bform) = bform

processInput : Maybe Input -> Widget1 -> Widget1
processInput input widget = case widget of
  Widget1 processor _ -> maybe widget processor input

editor focus = foldp processInput (normalModeWidget focus) input
