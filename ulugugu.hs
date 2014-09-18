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
import Focus

import Widget

{-/ WICHTIG \-}
type Boll = Bool
{-\ WICHTIG /-}


----------------- MAIN ------------------
main = runGTKWidget $ exprRendererSystem someEnv $
  localDefinitionsWidgetStd $ renderValueWrapper $ variableWidget 4

----------------- FORM -------------------

ourTextStyle = defaultTextStyle { fontSize = 23 }

renderShow :: (Show s) => s -> Form
renderShow = renderString . show

renderString :: String -> Form
renderString = text ourTextStyle

renderError :: String -> Form
renderError = text ourTextStyle { textColor = red } 

debugEnv color form
  = padded 4 <|
      (outlined ((solid color) { lineWidth = 2 }) . fromEnvelope . fEnvelope $ form)
      `atop` form


besides ls = centeredX $ groupBy toRight ls
besides2 x y = besides [x,y]

x `above` y = groupBy toBottom [x, y]


---------------- WIDGET STUFF ---------------

(Just x) `orTry` somethingElse = Just x
Nothing  `orTry` somethingElse = somethingElse


---------------- FOCUSED WIDGET -------------------

data WidgetFace = WidgetFace { focusedWidgetFace   :: Form
                             , unfocusedWidgetFace :: Form
                             }

rendererToWidgetFace renderer
  = WidgetFace { focusedWidgetFace = renderer True
               , unfocusedWidgetFace = renderer False
               }

mapWidgetFaces onFocused onUnfocused (WidgetFace focused unfocused)
  = WidgetFace (onFocused focused) (onUnfocused unfocused)

mapWidgetFace f = mapWidgetFaces f f

type FocusedWidget event value = Widget event (WidgetFace, value)

focusedRendererSystem :: FocusedWidget GtkEvent value -> Widget GtkEvent Form
focusedRendererSystem = mapValueOfWidget (focusedWidgetFace . fst)

constWidget :: WidgetFace -> value -> FocusedWidget event value
constWidget widgetFace value
  = Widget (const Nothing) (widgetFace, value)

emptyWidget :: value -> FocusedWidget event value
emptyWidget = constWidget $ WidgetFace emptyForm emptyForm 

labelWidget :: String -> value -> FocusedWidget event value
labelWidget name value
  = constWidget 
      WidgetFace { focusedWidgetFace = text ourTextStyle{ textColor = orange } name
                 , unfocusedWidgetFace = renderString name
                 }
      value


--------------- TEXT INPUT -----------------

renderTextInput :: TextInput -> Boll -> Form
renderTextInput (l,r) focused
  = border <| centeredX <| groupBy toRight [leftText, cursor, rightText]
  where
    leftText  = text ourTextStyle (reverse l)
    rightText = text ourTextStyle r
    cursor = if focused
                then rectangle 2 33 |> filled black |> alignY 0 |> flip withEnvelope emptyEnvelope
                else emptyForm
    border = padded 2 . debugEnv (if focused then orange else grey) . padded 2

interpretTextInput :: TextInput -> GtkEvent -> Maybe TextInput
interpretTextInput textInput (KeyPress key) = case key of
  Letter c           -> Just (textInputInsert c textInput)
  Special ArrLeft    -> textInputMoveLeft textInput
  Special ArrRight   -> textInputMoveRight textInput
  Special Backspace  -> textInputDelete textInput
  _                  -> Nothing
interpretTextInput _ _ = Nothing

textInputWidget :: TextInput -> FocusedWidget GtkEvent String
textInputWidget init
  = Widget (liftM textInputWidget . interpretTextInput init)
           (rendererToWidgetFace (renderTextInput init), fromTextInput init)




-------------- INT WIDGET --------------

maybeRead :: Read r => String -> Maybe r
maybeRead str = case reads str of
 [(x,"")] -> Just x
 _        -> Nothing

intWidget :: Int -> FocusedWidget GtkEvent Int
intWidget num
  = Widget step (face,num)
  where
    step (KeyPress (Letter ',')) = Just $ intWidget (num - 1)
    step (KeyPress (Letter '.')) = Just $ intWidget (num + 1)
    step _ = Nothing

    face = WidgetFace
             {focusedWidgetFace   = text ourTextStyle { textColor = orange } (show num)
             ,unfocusedWidgetFace = text ourTextStyle (show num)
             }



------------- EXPR WIDGET -------------

type ExprWidget value = Env -> FocusedWidget ExprWidgetEvent value

data ExprWidgetEvent = InputEvent GtkEvent
                     | EnvEvent Env

onInputEvents f (InputEvent event) = f event
onInputEvents f _ = Nothing

exprRendererSystem :: Env -> ExprWidget value -> Widget GtkEvent Form
exprRendererSystem initialEnv widget
  = preprocessEventsToWidget InputEvent $
      mapValueOfWidget (focusedWidgetFace . fst) $
        widget initialEnv

type Value = Int
type Id = Int

-- Pihmel, wir brauchen textuelle identifikation
type Env = Id -> Maybe (Form, Value)

emptyEnv :: Env
emptyEnv = const Nothing

addBinding :: Id -> Form -> Value -> Env -> Env
addBinding id form value env = \ lookupId ->
  if lookupId == id
     then Just (form, value)
     else lookupEnv lookupId env

lookupEnv :: Id -> Env -> Maybe (Form, Value)
lookupEnv lookupId env = env lookupId

listToEnv :: [Env -> Env] -> Env
listToEnv ls = accum emptyEnv ls
  where
    accum = foldr ($)

someEnv :: Env
someEnv = listToEnv $
  [addBinding 0 (renderString "twelve") 12
  ]

variableWidget :: Id -> ExprWidget Value
variableWidget varid env
  = Widget step (face, value)
  where
    step (EnvEvent newEnv) = Just $ variableWidget varid newEnv
    step _ = Nothing

    value = fromJust $ liftM snd $ lookupEnv varid env

    face = rendererToWidgetFace render
    render focused = liftM (debugEnv color . fst) (lookupEnv varid env)
                     `orElse` renderError "variable not found"
      where color = if focused then orange else grey

renderValueWrapper :: Show value => ExprWidget value -> ExprWidget value
renderValueWrapper widget env
  = renderValueWrapper' (widget env)

renderValueWrapper' :: Show value => FocusedWidget event value -> FocusedWidget event value
renderValueWrapper' widget
  = Widget step (faceWithValue, value)
  where
    step = liftM renderValueWrapper' . stepWidget widget

    faceWithValue = mapWidgetFace addRenderedValue widgetFace
    addRenderedValue widget = renderValue value `above` widget

    (widgetFace, value) = valueOfWidget widget

localDefinitionsWidget :: Focus String
                       -> FocusedWidget ExprWidgetEvent value
                       -> FocusedWidget GtkEvent String
                       -> FocusedWidget GtkEvent Int
                       -> ExprWidget value
localDefinitionsWidget focus oldExprWidget textWidget valueWidget env
  = Widget step (face, exprValue)
  where
    step event = (case getFocused focus of
                    "expr"  -> stepExprWidget event
                    "text"  -> onInputEvents stepTextWidget event
                    "value" -> onInputEvents stepValueWidget event)
                 `orTry` onInputEvents progressOuter event

    stepExprWidget event = do
      newExprWidget <- stepWidget exprWidget event
      return $ localDefinitionsWidget focus newExprWidget textWidget valueWidget env
    stepTextWidget event = do
      newTextWidget <- stepWidget textWidget event
      return $ localDefinitionsWidget focus exprWidget newTextWidget valueWidget env
    stepValueWidget event = do
      newValueWidget <- stepWidget valueWidget event
      return $ localDefinitionsWidget focus exprWidget textWidget newValueWidget env

    progressOuter (KeyPress (Letter 'l'))
      = do newFocus <- moveRight focus
           return $ localDefinitionsWidget newFocus exprWidget textWidget valueWidget env
    progressOuter (KeyPress (Special ArrRight))
      = do newFocus <- moveRight focus
           return $ localDefinitionsWidget newFocus exprWidget textWidget valueWidget env
    progressOuter (KeyPress (Letter 'h'))
      = do newFocus <- moveLeft focus
           return $ localDefinitionsWidget newFocus exprWidget textWidget valueWidget env
    progressOuter (KeyPress (Special ArrLeft))
      = do newFocus <- moveLeft focus
           return $ localDefinitionsWidget newFocus exprWidget textWidget valueWidget env
    progressOuter _ = Nothing

    face = rendererToWidgetFace render

    render focused = besides [exprForm
                             ,rectangle 3 100 |> filled black
                             ,varNameForm
                             ,renderString " = "
                             ,varValueForm
                             ]
      where
        getForm = if focused then focusedWidgetFace else unfocusedWidgetFace
        getForms = case getFocused focus of
          "expr"  -> [getForm,             unfocusedWidgetFace, unfocusedWidgetFace]
          "text"  -> [unfocusedWidgetFace, getForm,             unfocusedWidgetFace]
          "value" -> [unfocusedWidgetFace, unfocusedWidgetFace, getForm]
        [exprForm, varNameForm, varValueForm]
          = zipWith ($) getForms [exprFace, varNameFace, varValueFace]
        
    newEnv = addBinding 4 (renderString varName) varValue env
    exprWidget = stepWidget oldExprWidget (EnvEvent newEnv) `orElse` oldExprWidget
    (exprFace,     exprValue) = valueOfWidget exprWidget
    (varNameFace,  varName)   = valueOfWidget textWidget
    (varValueFace, varValue)  = valueOfWidget valueWidget

localDefinitionsWidgetStd :: ExprWidget value -> ExprWidget value
localDefinitionsWidgetStd exprWidget env
  = localDefinitionsWidget (focus ["expr", "text", "value"])
                           (exprWidget newEnv)
                           (textInputWidget (toTextInput varName))
                           (intWidget varValue)
                           env
  where
    newEnv = addBinding 4 (renderString varName) varValue env
    varName = "pihmel"
    varValue = 0

renderValue x = renderShow x
