import Window

import Bounded (..)
import DrawingUtils (..)
--import Widget (..)
import Automaton (..)

import AST (..)

import TextInput (..)
import Input (..)
import MaybeUtils (..)

----- STUFF -----

unzip3 : [(a,b,c)] -> ([a],[b],[c])
unzip3 ls = case ls of
  [] -> ([], [], [])
  ((x,y,z) :: rest) -> let (xs, ys, zs) = unzip3 rest
                       in (x::xs, y::ys, z::zs)

fst3 : (a,b,c) -> a
fst3 (x,y,z) = x

elem : a -> [a] -> Bool
elem x = any ((==) x)

------ TESTING ------

type Widget1 = Automaton (Event Input) (Behavior (Bounded Form))

formWidget : Bounded Form -> Widget1
formWidget bform = constBehavior bform

textWidget : String -> Widget1
textWidget text = formWidget (btext text)

nodeWidget : LineStyle -> Bounded Form -> Widget1
nodeWidget linestyle content = formWidget (drawNode linestyle content)

drawTree : Bounded Form -> Bounded Form -> Bounded Form
drawTree parentForm childForm = parentForm `connectChildren` [childForm]

textInputWidget : TextInput -> Widget1
textInputWidget textinput =
  let processInput : Input -> TextInput -> TextInput
      processInput i = case i of
                         Letter c          -> textInputInsert c
                         Special Backspace -> foldMaybe textInputDelete
                         Special ArrLeft   -> foldMaybe textInputMoveLeft
                         Special ArrRight  -> foldMaybe textInputMoveRight
                         _                 -> id

      renderTextInput : TextInput -> Bounded Form
      renderTextInput (prev,after) = 
        let drawChars = btext . String.fromList
            cursor    = bsegment (0,-9) (0,9) $> traced (solid black)
        in alignWith (alignXOffset 0.5) <| besides [drawChars (reverse prev), cursor, drawChars after]
  in foldEvents processInput textinput >>> mapBehavior renderTextInput


drawCollage : (Int,Int) -> Bounded Form -> Element
drawCollage (w,h) bform
  = container w h middle <| collageBounded
                         <| padAll 15 <| bscale 2
                         <| bform

besides2 : Bounded Form -> Bounded Form -> Bounded Form
besides2 x y = besides [x,y]

focusWrapper : Widget1 -> Widget1
focusWrapper widget =
  let processInput : Input -> Bool -> Bool
      processInput input state = case input of
        Letter 'i' -> True
        Special Escape -> False
        _ -> state

      shouldIFilter : Automaton (Event Input) (Behavior Bool)
      shouldIFilter = foldEvents processInput False

      zippedInput : Automaton (Event Input) (Behavior Bool, Event Input)
      zippedInput = fork shouldIFilter identity

      filteredInput : Automaton (Event Input) (Event Input)
      filteredInput = keepWhen <<< zippedInput
  in widget <<< filteredInput

constWidget : Bounded Form -> Automaton (Event Input) (Event Input, Behavior (Bounded Form))
constWidget bform = fork identity (constBehavior bform)

modeCircle : Automaton (Event Input) (Event Input, Behavior (Bounded Form))
modeCircle = let renderCircle focussed = let color = if focussed then blue else green
                                         in bcircle 100 $> filled color
                 processInput i z = case i of
                   Letter 'i' -> True
                   Special Escape -> False
                   _ -> z
                 ischtRelefant x = case x of
                   Letter 'i' -> False
                   Special Escape -> False
                   _ -> True
             in fork (filterEvents ischtRelefant)
                     (foldEvents processInput False >>> mapBehavior renderCircle)

containe1 : Automaton (Event Input) (Event Input, Behavior (Bounded Form))
         -> Automaton (Event Input) (Event Input, Behavior (Bounded Form))
containe1 widget = let isFocussed : Automaton (Event Input) (Behavior Bool)
                       isFocussed = foldEvents (\ i state -> case i of
                                                               Letter 'i' -> True
                                                               Special Escape -> False
                                                               _ -> state)
                                               False

                       machdesrichtig i = case i of
                         Letter 'i' -> Left (always True)
                         Special Escape -> Left (always False)
                         _ -> Right i

                       machdeserschtmalsauber (ev,(wev,wbf))
                         = (mergeEvents ev wev, wbf)

                       machdessauber ((ev,state),bild) =
                         (ev, onBehavior2 renderContainer state bild)

                       ischtNotRelefant x = case x of
                         Letter 'i' -> False
                         Special Escape -> False
                         _ -> True

                       renderContainer : Bool -> Bounded Form -> Bounded Form
                       renderContainer focussed bform =
                         let annotate = if focussed then debugEnvelope else id
                         in annotate bform
                   in swap
                      >>> fork (merge keepWhen) (merge dropWhen >>> widget)
                      >>> liftA machdeserschtmalsauber
                      >>> parallel (liftA machdesrichtig
                                    >>> splitEvents
                                    >>> parallel (accumEvents False) identity)
                                   identity
                      >>> liftA machdessauber

mainWidgetInit : Automaton (Event Input) (Event Input, Behavior (Bounded Form))
mainWidgetInit = containe1 modeCircle 

showLastEvent : Automaton (Event a) (Behavior (Bounded Form))
showLastEvent = let holdMaybeLast : Automaton (Event a) (Behavior (Maybe a))
                    holdMaybeLast = mapEvent Just >>> holdLast Nothing
                in holdMaybeLast >>> mapBehavior (maybe (bempty 0 0) (btext . show))

debugWidget : Automaton (Event Input, Behavior (Bounded Form))
                        (Behavior (Bounded Form))
debugWidget = parallel showLastEvent identity >>> merge (onBehavior2 besides2)

mainWidget : Signal (Bounded Form)
mainWidget = run (filterJust >>> mainWidgetInit >>> debugWidget) input

main : Signal Element
main = lift2 drawCollage Window.dimensions mainWidget
