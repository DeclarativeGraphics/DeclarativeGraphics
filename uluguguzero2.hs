{-# LANGUAGE NoMonomorphismRestriction, TupleSections, ExistentialQuantification #-}
module Main where

import Data.Maybe
import Data.List (replicate)

import FRPZero
import FRPZeroGtk
import GtkUtils

import Graphics.Declarative.Envelope
import Graphics.Declarative.Shape
import Graphics.Declarative.Form
import Graphics.Declarative.Combinators

import Control.Monad (liftM)

import Colors
import Utils
import TextInput
import KeyboardInput


ourTextStyle = defaultTextStyle { fontSize = 23 }

renderShow :: (Show s) => s -> Form
renderShow = renderString . show

renderString :: String -> Form
renderString = text ourTextStyle

besides ls = centered $ groupBy toRight ls
besides2 x y = centered $ groupBy toRight [x, y]

x `above` y = groupBy toBottom [x, y]

renderTextInput :: Bool -> TextInput -> Form
renderTextInput focused (l,r)
  = border <| centered <| groupBy toRight [centered leftText, cursor, centered rightText]
  where
    leftText  = text ourTextStyle (reverse l)
    rightText = text ourTextStyle r
    cursor = if focused
                then rectangle 2 27 |> filled black |> flip withEnvelope emptyEnvelope
                else emptyForm
    border = padded 2 . debugEnv (if focused then orange else grey) . padded 2


debugEnv color form
  = (outlined ((solid color) { lineWidth = 3 }) . fromEnvelope . fEnvelope $ form)
    `atop` form


interpretTextInput :: GtkEvent -> (TextInput -> Maybe TextInput)
interpretTextInput (KeyPress key) = case key of
  Letter c           -> Just . textInputInsert c
  Special ArrLeft    -> textInputMoveLeft
  Special ArrRight   -> textInputMoveRight
  Special Backspace  -> textInputDelete
  _                  -> const Nothing
interpretTextInput _ = const Nothing

type StepFunc state = GtkEvent -> state -> Maybe state
type RenderFunc state = Bool -> state -> Form

data Widget value
  = forall state .
      Widget state
             (StepFunc state)
             (RenderFunc state)
             (state -> value)

stepWidget :: GtkEvent -> Widget value -> Maybe (Widget value)
stepWidget event (Widget state step render valueOf)
  = do newState <- step event state
       return (Widget newState step render valueOf)

valueOfWidget :: Widget value -> value
valueOfWidget (Widget state step render valueOf) = valueOf state

renderWidget :: Bool -> Widget value -> Form
renderWidget focused (Widget state step render valueOf) = render focused state

textInputWidget init
  = Widget (toTextInput init) interpretTextInput renderTextInput fromTextInput

modeWrapper widget
  = Widget (False, widget) step render (valueOfWidget . snd)
  where
    step event (True, widget) = case progressInner event widget of
      Just newWidget -> Just (True, newWidget)
      Nothing        -> progressOuter event (True, widget)
    step event (False, widget) = progressOuter event (False, widget)

    progressInner = stepWidget

    progressOuter (KeyPress (Letter 'i')) (False, widget)
      = Just (True, widget)
    progressOuter (KeyPress (Special Escape)) (True, widget)
      = Just (False, widget)
    progressOuter _ _ = Nothing

    render True (False, widget)
      = debugEnv blue <| renderWidget False widget
    render True (True, widget)
      = renderWidget True widget
    render False (_, widget)
      = renderWidget False widget


modeInputWidget = modeWrapper . textInputWidget


{-
treeWidget parent children
    progressOuter (KeyPress (Special ArrLeft))  = moveTreeLeft
    progressOuter (KeyPress (Special ArrRight)) = moveTreeRight
    progressOuter (KeyPress (Special ArrDown))  = moveTreeDown
    progressOuter (KeyPress (Special ArrUp))    = moveTreeUp
    progressOuter (KeyPress (Letter 'a'))       = insertTree (modeInputWidget "")
    progressOuter (KeyPress (Letter 'x'))       = deleteTree
    progressOuter _                             = const Nothing
      -}

data Value = Num Int | Operator Operator deriving (Show,Eq)
data Operator = Add | Sub | Mul deriving (Show,Eq)

constWidget :: Form -> Form -> value -> Widget value
constWidget focusedForm unfocusedForm value
  = Widget () step render (const value)
  where
    step _ _ = Nothing
    render True _  = focusedForm
    render False _ = unfocusedForm

operatorExprWidget name operator
  = constWidget (text ourTextStyle{ textColor = orange } name)
                (renderString name) (Operator operator)

addOperatorExprWidget = operatorExprWidget "+" Add
subOperatorExprWidget = operatorExprWidget "-" Sub
mulOperatorExprWidget = operatorExprWidget "*" Mul
operatorsSwitchWidget
  = switchWidget [addOperatorExprWidget, subOperatorExprWidget, mulOperatorExprWidget]
intValueExprWidget = mapWidgetValue Num . intWidget

mapWidgetValue f (Widget state step render value)
  = Widget state step render (f . value)

switchWidget :: [Widget value] -> Widget value
switchWidget widgets
  = Widget (focus widgets) step render (valueOfWidget . getFocused)
  where
    step event focus = case progressInner event focus of
      Just newFocus -> Just newFocus
      Nothing       -> progressOuter event focus

    progressInner event focus
      = do newFocused <- stepWidget event (getFocused focus)
           return (setFocused newFocused focus)

    progressOuter (KeyPress (Special ArrLeft))  = moveLeft
    progressOuter (KeyPress (Special ArrRight)) = moveRight
    progressOuter _                             = const Nothing

    render focused = renderWidget focused . getFocused

treeWidget :: Widget parent -> [Widget value] -> Widget (parent, [value])
treeWidget parent children
  = Widget (True, parent, hlistWidget children) step render valueOf
  where
    step event focus = case progressInner event focus of
      Just newFocus -> Just newFocus
      Nothing       -> progressOuter event focus

    progressInner event (True, parent, children)
      = do newParent <- stepWidget event parent
           return $ (True, newParent, children)
    progressInner event (False, parent, children)
      = do newChildren <- stepWidget event children
           return $ (False, parent, newChildren)

    progressOuter (KeyPress (Special ArrUp))   (False, p, c) = Just (True, p, c)
    progressOuter (KeyPress (Special ArrDown)) (True, p, c)  = Just (False, p, c)
    progressOuter _                            _             = Nothing

    apply2List f [x,y] = f x y

    render focused (True, parent, children)
      = renderWidget focused parent `above` renderWidget False children
    render focused (False, parent, children)
      = renderWidget False parent `above` renderWidget focused children

    valueOf (_, parent, children) = (valueOfWidget parent, valueOfWidget children)

applicationWidget parent children
  = mapWidgetValue (uncurry applyOperator) $ treeWidget parent children
  where
    applyOperator (Operator op) args = operate op args
    applyOperator op            args = Left $ "invalid Operator: " ++ show op

    operate Add [Num x, Num y] = Right $ Num (x + y)
    operate Sub [Num x, Num y] = Right $ Num (x - y)
    operate Mul [Num x, Num y] = Right $ Num (x * y)

    operate _   args           = Left $ "invalid arguments: " ++ show args

intWidget :: Int -> Widget Int
intWidget def
  = Widget def step render id
  where step (KeyPress (Special ArrLeft))  x = Just (x - 1)
        step (KeyPress (Special ArrRight)) x = Just (x + 1)
        step _ _ = Nothing

        render True  value = text ourTextStyle { textColor = orange } (show value)
        render False value = text ourTextStyle (show value)
        

hlistWidget :: [Widget value] -> Widget [value]
hlistWidget widgets
  = Widget (focus widgets) step render valueOf
  where
    step event focus = case progressInner event focus of
      Just newFocus -> Just newFocus
      Nothing       -> progressOuter event focus

    progressInner event focus = do newWidget <- stepWidget event focusedWidget
                                   return $ setFocused newWidget focus
      where focusedWidget = getFocused focus

    {-
    progressOuter :: GtkEvent
                  -> Focus (Widget ...)
                  -> Maybe (Focus (Widget ...))
                  -}
    progressOuter (KeyPress (Special ArrLeft))  = moveLeft
    progressOuter (KeyPress (Special ArrRight)) = moveRight
    --progressOuter (KeyPress (Letter 'a'))       = insertFocus (modeInputWidget "")
    progressOuter (KeyPress (Letter 'x'))       = deleteFocus
    progressOuter _                             = const Nothing

    render focused
      = besides . focusAsList . mapFocusParts (renderWidget False) (renderWidget focused) (renderWidget False)

    valueOf = map valueOfWidget . focusAsList

widgetAsState :: Show value => Widget value -> State GtkEvent Form
widgetAsState (Widget state step render valueOf)
  = (maybeApply . step)
    >>^ accum state ^>>
    biapply above (render True) (centered . renderShow . valueOf)

biapply merge f g x = merge (f x) (g x)


type Tree a = (Bool,a,Focus a)

treeFocus parent children = (True, parent, focus children)

getTreeFocused (True, parent, _) = parent
getTreeFocused (False, _, children) = getFocused children

setTreeFocused x = onTreeFocused (const x)

onTreeFocused f (True, parent, children) = (True, f parent, children)
onTreeFocused f (False, parent, children) = (False, parent, onFocused f children)

insertTree x (True, _, _) = Nothing
insertTree x (False, parent, children) = do newChildren <- insertFocus x children
                                            return (False, parent, newChildren)

onChildren f (focus, parent, children) = (focus, parent, f children)

moveTreeRight (True, _, _) = Nothing
moveTreeRight (False, parent, children) = do newChildren <- moveRight children
                                             return (False, parent, newChildren)

moveTreeLeft (True, _, _) = Nothing
moveTreeLeft (False, parent, children) = do newChildren <- moveLeft children
                                            return (False, parent, newChildren)

moveTreeUp (True, _, _) = Nothing
moveTreeUp (False, parent, children) = Just (True, parent, children)

moveTreeDown (False, _, _) = Nothing
moveTreeDown (True, parent, children) = Just (False, parent, children)

deleteTree (True, _, _) = Nothing
deleteTree (False, parent, children) = do newChildren <- deleteFocus children
                                          return (True, parent, newChildren)


type Focus a = ([a],a,[a])

focus (focus:more) = ([],focus,more)
getFocused (_,focus,_) = focus
setFocused x = onFocused (const x)
focusAsList (left, focus, right) = reverse left ++ [focus] ++ right

moveRight (left, focus, x:right) = Just (focus:left, x, right)
moveRight (left, focus, [])      = Nothing

moveLeft (x:left, focus, right) = Just (left, x, focus:right)
moveLeft ([], focus, right)     = Nothing

onFocused f (left, focus, right) = (left, f focus, right)
onFocusedLifted f (left, focus, right) = do newFocus <- f focus
                                            return (left, newFocus, right)
mapFocus f (left, focus, right) = (map f left, f focus, map f right)

mapFocusParts leftF focusedF rightF (left, focus, right)
  = (map leftF left, focusedF focus, map rightF right)

insertFocus x (left, focus, right) = Just (focus:left, x, right)

deleteFocus (left, focus, r:right) = Just (left, r, right)
deleteFocus (l:left, focus, right) = Just (left, l, right)
deleteFocus _                      = Nothing


-- step function refactorisieren!!!!!

main = runGtkZero $ widgetAsState $
  applicationWidget
    # operatorsSwitchWidget
    # replicate 2 (modeWrapper $ intValueExprWidget 0)
{-
  treeWidget
    # modeInputWidget "parent"
    # replicate 5 (modeInputWidget "1")
-}
{-
  treeWidget (modeInputWidget "Halloechen, Fadda") $
    [treeWidget (modeInputWidget "Subfadda") $
       [modeInputWidget "Pihmel", modeInputWidget "Votze"]
    , modeWrapper $ modeInputWidget "Vagihna"
    ]
    -}
