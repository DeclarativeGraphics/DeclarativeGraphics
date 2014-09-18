{-# LANGUAGE NoMonomorphismRestriction, TupleSections, ExistentialQuantification #-}
module Main where

import Data.Maybe
import Data.Either
import Data.List (replicate, partition)

import GtkUtils

import Graphics.Declarative.Envelope
import Graphics.Declarative.Shape
import Graphics.Declarative.Form
import Graphics.Declarative.Combinators

import Control.Monad (liftM, liftM2)

import Colors
import Utils
import TextInput
import KeyboardInput


intWidget :: Int -> Widget (Maybe Int)
intWidget def
  = widget (Left def) step render value
  where step (KeyPress (Letter 'i')) (Left x)
          = Just (Right (textInputWidget (show x)))
        step event (Left x)
          = do newX <- stepNormalMode event x
               return (Left newX)
        step event (Right state)
          = do newState <- stepInsertMode event state
               return newState

        stepNormalMode (KeyPress (Letter '-')) x = Just (x - 1)
        stepNormalMode (KeyPress (Letter ',')) x = Just (x - 1)
        stepNormalMode (KeyPress (Letter '+')) x = Just (x + 1)
        stepNormalMode (KeyPress (Letter '.')) x = Just (x + 1)
        stepNormalMode  _ _ = Nothing

        stepInsertMode event textInput = case stepWidget event textInput of
          Just newTextInput -> Just (Right newTextInput)
          Nothing -> case event of
            KeyPress (Special Return) -> Just switchMode
            KeyPress (Special Escape) -> Just switchMode
            _ -> Nothing
          where
            switchMode = maybe (Right textInput) Left (textInputNumber textInput)

        textInputNumber :: Widget String -> Maybe Int
        textInputNumber = maybeRead . valueOfWidget

        render focused (Left x)          = renderNormalMode focused x
        render focused (Right textInput) = renderInsertMode focused textInput

        renderNormalMode True  value = text ourTextStyle { textColor = orange } (show value)
        renderNormalMode False value = text ourTextStyle (show value)

        renderInsertMode focused textInput
          = addErrorMessage $ renderWidget focused textInput
          where
            addErrorMessage = case textInputNumber textInput of
              Just _  -> id
              Nothing -> \ form ->
                groupBy toTop [form
                              ,centeredX $ text ourTextStyle { textColor = red } "invalid Int"
                              ]

        value = either Just textInputNumber


modeWrapper' :: (Bool -> (Bool, Widget value) -> Form) -> Widget value -> Widget value
modeWrapper' render wrappedWidget
  = widget (False, wrappedWidget) step render (valueOfWidget . snd)
  where
    step event (True, wrappedWidget) = case progressInner event wrappedWidget of
      Just newWidget -> Just (True, newWidget)
      Nothing        -> progressOuter event (True, wrappedWidget)
    step event (False, wrappedWidget) = progressOuter event (False, wrappedWidget)

    progressInner = stepWidget

    progressOuter (KeyPress (Letter 'i')) (False, wrappedWidget)
      = Just (True, wrappedWidget)
    progressOuter (KeyPress (Special Escape)) (True, wrappedWidget)
      = Just (False, wrappedWidget)
    progressOuter _ _ = Nothing

modeWrapper = modeWrapper' render
  where
    render focused (insertMode, wrappedWidget)
      = debugEnv color <| renderWidget (focused && insertMode) wrappedWidget
      where
        color = if focused && not insertMode then lightOrange else lightGrey


modeInputWidget :: String -> Widget String
modeInputWidget = modeWrapper . textInputWidget

type Boll = Bool

data Value = Num Int | Operator Operator | Prim Primitive deriving (Show,Eq)
data Operator = Add | Sub | Mul deriving (Show,Eq)
data Primitive = Primitive Name PrimFunc
type PrimFunc = [Value] -> Interpreted Value
type Name = String
type Interpreted value = Either ErrorMsg value
type ErrorMsg = String

showValue (Num int)     = show int
showValue (Operator op) = show op
showValue (Prim prim)   = show prim

runPrimitive (Primitive name function) args = function args

instance Show Primitive where
  show (Primitive name function) = name
instance Eq Primitive where
  _ == _ = False

operatorExprWidget :: String -> Operator -> Widget Value
operatorExprWidget name operator = labelWidget name (const (Operator operator))
primitiveExprWidget :: String -> PrimFunc -> Widget Primitive
primitiveExprWidget name function = labelWidget name (const (Primitive name function))

addPrimitive, subPrimitive, mulPrimitive :: PrimFunc
addPrimitive [Num x, Num y] = Right $ Num (x + y)
addPrimitive args           = Left  $ "invalid arguments for primitive `+`: " ++ show args
subPrimitive [Num x, Num y] = Right $ Num (x - y)
subPrimitive args           = Left  $ "invalid arguments for primitive `-`: " ++ show args
mulPrimitive [Num x, Num y] = Right $ Num (x * y)
mulPrimitive args           = Left  $ "invalid arguments for primitive `*`: " ++ show args

addPrimitiveExprWidget, subPrimitiveExprWidget, mulPrimitiveExprWidget :: Widget Primitive
addPrimitiveExprWidget = primitiveExprWidget "+" addPrimitive
subPrimitiveExprWidget = primitiveExprWidget "-" subPrimitive
mulPrimitiveExprWidget = primitiveExprWidget "*" mulPrimitive
primitivesSwitchWidget :: Widget Value
primitivesSwitchWidget
  = mapWidgetValue Prim $
      switchWidgetComma
        [addPrimitiveExprWidget, subPrimitiveExprWidget, mulPrimitiveExprWidget]

addOperatorExprWidget, subOperatorExprWidget, mulOperatorExprWidget :: Widget Value
addOperatorExprWidget = operatorExprWidget "Add" Add
subOperatorExprWidget = operatorExprWidget "Sub" Sub
mulOperatorExprWidget = operatorExprWidget "Mul" Mul
operatorsSwitchWidget :: Widget Value
operatorsSwitchWidget
  = switchWidgetComma
      [addOperatorExprWidget, subOperatorExprWidget, mulOperatorExprWidget]

exprSwitchWidgetExtend wrappedWidget
  = extendWidget exprSwitchWidgetExtend $
      switchWidget' [KeyPress (Letter 'n')] [KeyPress (Letter 'm')] $
        [modeWrapper' renderInterpretedValue $
           operatorWidget exprSwitchWidget extendedWidget exprSwitchWidget
        ,modeWrapper $ applicationWidget exprSwitchWidget exprSwitchWidget [extendedWidget]
        ,mapWidgetValue Just primitivesSwitchWidget
        ,mapWidgetValue Just operatorsSwitchWidget
        ,intValueExprWidget 0
        ]
  where
    extendedWidget = extendWidget exprSwitchWidgetExtend wrappedWidget

exprSwitchWidget
  = extendWidget exprSwitchWidgetExtend $
      switchWidget' [KeyPress (Letter 'n')] [KeyPress (Letter 'm')] $
        [intValueExprWidget 0
        ,mapWidgetValue Just primitivesSwitchWidget
        ,mapWidgetValue Just operatorsSwitchWidget
        ,modeWrapper $ applicationWidget exprSwitchWidget
                                         exprSwitchWidget [exprSwitchWidget]
        ,modeWrapper' renderInterpretedValue $
           operatorWidget exprSwitchWidget exprSwitchWidget exprSwitchWidget
        ]


intValueExprWidget
  = mapWidgetValue (liftM (const . Num)) . intWidget

renderInterpretedValue focused (insertMode, wrappedWidget)
  = groupBy toBottom [widgetForm
                     ,renderValue (centered line)
                     ]
  where
    line = rectangle widgetWidth height |> filled lineColor
    widgetForm = renderWidget (insertMode && focused) wrappedWidget
    widgetEnvelope = fEnvelope widgetForm
    widgetWidth = envToRight widgetEnvelope - envToLeft widgetEnvelope
    height = 10
    lineColor = if focused
                   then (if insertMode then darkGrey else orange)
                   else grey
    tColor = Colors.white
    renderValue = atop $ renderString valueText
    valueText = maybe "no value" showValue $ valueOfWidget wrappedWidget
    renderString
      = centered . text defaultTextStyle { fontSize = height, textColor = tColor, bold = True }


mapWidgetValue f (Widget step render value)
  = Widget (liftM (mapWidgetValue f) . step) render (f value)

extendWidget :: (Widget value -> Widget value) -> Widget value -> Widget value
extendWidget extend wrappedWidget
  = Widget step (renderWidget' wrappedWidget) (valueOfWidget wrappedWidget)
  where
    step event = case stepWidget event wrappedWidget of
      Just newWidget -> Just (extendWidget extend newWidget)
      Nothing        -> case event of
        KeyPress (Letter 'e') -> Just (extend wrappedWidget)
        _                     -> Nothing

switchWidget = switchWidget' (map KeyPress [Special ArrLeft, Letter 'h'])
                             (map KeyPress [Special ArrRight, Letter 'l'])

switchWidgetComma = switchWidget' [KeyPress (Letter ',')] [KeyPress (Letter '.')]

switchWidget' :: [GtkEvent] -> [GtkEvent] -> [Widget value] -> Widget value
switchWidget' prevEvent nextEvent widgets
  = widget (focus widgets) step render (valueOfWidget . getFocused)
  where
    step event focus = case progressInner event focus of
      Just newFocus -> Just newFocus
      Nothing       -> progressOuter event focus

    progressInner event focus
      = do newFocused <- stepWidget event (getFocused focus)
           return (setFocused newFocused focus)

    progressOuter event = if      event `elem` prevEvent then Just . maybeApply moveLeft
                          else if event `elem` nextEvent then Just . maybeApply moveRight
                          else                                const Nothing

    render focused = renderWidget focused . getFocused

chooseWidget :: [(GtkEvent, Widget value)] -> Widget value
chooseWidget (chosenWidget : otherWidgets)
  = widget (chosenWidget, otherWidgets)
           step
           render
           (valueOfWidget . snd . fst)
  where
    step event ((chooseEvent, chosenWidget), otherWidgets)
      = case stepWidget event chosenWidget of
          Just newChosenWidget -> Just ((chooseEvent, newChosenWidget), otherWidgets)
          Nothing -> case partition ((== event) . fst) otherWidgets of
                       ([(newChooseEvent, newChosenWidget)], otherWidgets)
                         -> Just ((newChooseEvent, newChosenWidget)
                                 ,(chooseEvent, chosenWidget) : otherWidgets)
                       _ -> Nothing
    
    render focused = renderWidget focused . snd . fst

treeWidget :: Widget parentvalue
           -> Widget childvalue
           -> [Widget childvalue]
           -> Widget (parentvalue, [childvalue])
treeWidget parentWidget newChildWidget childWidgets
  = widget (True, parentWidget, childrenWidget) step render valueOf
  where
    childrenWidget
      = chooseWidget [(KeyPress (Letter 'a'), hlistWidget newChildWidget childWidgets)
                     ,(KeyPress (Letter 'x'), emptyWidget [])
                     ]
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
    progressOuter (KeyPress (Letter 'k'))      (False, p, c) = Just (True, p, c)
    progressOuter (KeyPress (Special ArrDown)) (True, p, c)  = Just (False, p, c)
    progressOuter (KeyPress (Letter 'j'))      (True, p, c)  = Just (False, p, c)
    progressOuter _                            _             = Nothing

    apply2List f [x,y] = f x y

    render focused (True, parent, children)
      = centered (renderWidget focused parent) `above` renderWidget False children
    render focused (False, parent, children)
      = centered (renderWidget False parent) `above` renderWidget focused children

    valueOf (_, parent, children) = (valueOfWidget parent, valueOfWidget children)

operatorWidget operator leftarg rightarg
  = widget ("operator", operator, leftarg, rightarg) step render value
  where
    step event state = case progressInner event state of
      Just newState -> Just newState
      Nothing       -> progressOuter event state

    progressInner event ("operator", operator, leftarg, rightarg)
      = do newOperator <- stepWidget event operator
           return ("operator", newOperator, leftarg, rightarg)
    progressInner event ("left", operator, leftarg, rightarg)
      = do newLeftarg <- stepWidget event leftarg
           return ("left", operator, newLeftarg, rightarg)
    progressInner event ("right", operator, leftarg, rightarg)
      = do newRightarg <- stepWidget event rightarg
           return ("right", operator, leftarg, newRightarg)

    progressOuter (KeyPress (Letter 'h')) ("right", o, l, r)
      = Just ("operator", o, l, r)
    progressOuter (KeyPress (Letter 'h')) ("operator", o, l, r)
      = Just ("left", o, l, r)

    progressOuter (KeyPress (Letter 'l')) ("left", o, l, r)
      = Just ("operator", o, l, r)
    progressOuter (KeyPress (Letter 'l')) ("operator", o, l, r)
      = Just ("right", o, l, r)

    progressOuter (KeyPress (Letter 'X')) (focus, o, l, r)
      = Just (focus, o, r, l)

    progressOuter _ _ = Nothing

    render focused state
      = renderError (interpret state) $
          besides $ renderParts focused state
    renderParts focused ("left", o, l, r) = [renderWidget focused l
                                            ,renderWidget False o
                                            ,renderWidget False r]
    renderParts focused ("operator", o, l, r) = [renderWidget False l
                                                ,renderWidget focused o
                                                ,renderWidget False r]
    renderParts focused ("right", o, l, r) = [renderWidget False l
                                             ,renderWidget False o
                                             ,renderWidget focused r]

    renderError (Just (Left error)) rest
      = groupBy toTop [rest
                      ,centeredX $ text ourTextStyle { textColor = red } error
                      ]
    renderError _ rest = rest

    interpret (_, o, l, r) = applyOperator (valueOfWidget o) (map valueOfWidget [l, r])

    value = interpretedToMaybe . interpret

interpretedToMaybe (Just (Right v)) = Just v
interpretedToMaybe _                = Nothing

applicationWidget parent newChild children
  = widget (treeWidget parent newChild children) stepWidget render value
  where
    render focused state
      = centered (renderInterpreted (interpret state))
        `above`
        renderWidget focused state
    interpret = uncurry applyOperator . valueOfWidget
    value = interpretedToMaybe . interpret

applyOperator op args = liftM2 applyOperator' op (sequence args)
applyOperator' (Operator op) args = operate op args
applyOperator' (Prim prim)   args = runPrimitive prim args
applyOperator' op            args = Left $ "invalid Operator: " ++ show op

operate Add [Num x, Num y] = Right $ Num (x + y)
operate Sub [Num x, Num y] = Right $ Num (x - y)
operate Mul [Num x, Num y] = Right $ Num (x * y)
operate _   args           = Left $ "invalid arguments: " ++ show args


hlistWidget :: Widget value -> [Widget value] -> Widget [value]
hlistWidget newWidget widgets
  = widget (focus widgets) step render valueOf
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
    progressOuter (KeyPress (Letter 'h'))       = moveLeft
    progressOuter (KeyPress (Special ArrRight)) = moveRight
    progressOuter (KeyPress (Letter 'l'))       = moveRight
    progressOuter (KeyPress (Letter 'H'))       = moveElementLeft
    progressOuter (KeyPress (Letter 'L'))       = moveElementRight
    progressOuter (KeyPress (Letter 'a'))       = insertFocus newWidget
    progressOuter (KeyPress (Letter 'x'))       = deleteFocus
    progressOuter _                             = const Nothing

    render focused
      = besides . focusAsList . mapFocusParts (renderWidget False) (renderWidget focused) (renderWidget False)

    valueOf = map valueOfWidget . focusAsList

renderInterpreted = maybe emptyForm renderInterpreted'
renderInterpreted' (Right value) = renderString (showValue value)
renderInterpreted' (Left error)  = text ourTextStyle { textColor = red } error

biapply merge f g x = merge (f x) (g x)

-- step function refactorisieren!!!!!

doubleApply f x = f x x

main = runGTKWidget $
{-
  doubleApply constWidget (debugEnvelope form) ()
  where
    form = rectangle 40 40 |> filled black |> alignY 0
    -}
  exprSwitchWidget
{-
  operatorWidget (modeWrapper primitivesSwitchWidget)
    (modeWrapper $ intValueExprWidget 0)
    (modeWrapper $ intValueExprWidget 0)
-}
{-
  applicationWidget
    # operatorsSwitchWidget -- oder primitivesSwitchWidget
    # replicate 2 (modeWrapper $ intValueExprWidget 0)
    -}
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
