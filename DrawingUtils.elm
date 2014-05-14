module DrawingUtils where

import Window

import Bounded (..)

----- DRAWING -----

drawNode : LineStyle -> Bounded Form -> Bounded Form
drawNode linestyle content = 
  let innerElement = content |> pad 5 5 5 5 |> alignWith (alignXOffset 0.5)
                                            |> alignWith (alignYOffset 0.5)
      (w,h) = bsize innerElement
      drawing = overlayBottomUp [brect w h $> outlined linestyle
                                ,brect w h $> filled white
                                ,innerElement]
  in drawing


toRight : Envelope -> Envelope -> (Float, Float)
toRight leftNeighbor rightNeighbor =
  (envelopeRight leftNeighbor - envelopeLeft rightNeighbor, 0)

toLeft : Envelope -> Envelope -> (Float, Float)
toLeft rightNeighbor leftNeighbor =
  (envelopeLeft rightNeighbor - envelopeRight leftNeighbor, 0)

toTop : Envelope -> Envelope -> (Float, Float)
toTop bottomNeighbor topNeighbor =
  (0, envelopeTop bottomNeighbor - envelopeBottom topNeighbor)

toBottom : Envelope -> Envelope -> (Float, Float)
toBottom topNeighbor bottomNeighbor =
  (0, envelopeBottom topNeighbor - envelopeTop bottomNeighbor)


addOffset : (Float,Float)
         -> (Envelope -> Envelope -> (Float,Float))
         -> (Envelope -> Envelope -> (Float,Float))
addOffset offset dist = \e1 e2 -> vectAdd offset (dist e1 e2)

vectAdd : (Float,Float) -> (Float,Float) -> (Float,Float)
vectAdd (x1,y1) (x2,y2) = (x1+x2, y1+y2)


attachWith : (Envelope -> Envelope -> (Float,Float)) -> Bounded Form -> Bounded Form -> Bounded Form
attachWith distance formToMove referenceForm =
  let formDistance = distance (envelope referenceForm) (envelope formToMove)
  in formToMove |> moveBy formDistance

attachList : (Envelope -> Envelope -> (Float,Float)) -> Bounded Form -> [Bounded Form] -> [Bounded Form]
attachList distance referenceForm formsToMove =
  let formDistance = distance (envelope referenceForm) (combineEnvelopes (map envelope formsToMove))
  in formsToMove |> map (moveBy formDistance)


above : Bounded Form -> Bounded Form -> Bounded Form
above topForm bottomForm =
  overlayTopDown [attachWith toTop topForm bottomForm, bottomForm]


attachAll : (Bounded Form -> Bounded Form -> Bounded Form) -> Bounded Form -> [Bounded Form] -> [Bounded Form]
attachAll attach referenceForm forms = tail <| scanl attach referenceForm forms

attachAll1 : (Bounded Form -> Bounded Form -> Bounded Form) -> [Bounded Form] -> [Bounded Form]
attachAll1 attach forms = scanl1 attach forms


alignXOffset : Float -> Envelope -> (Float,Float)
alignXOffset x envelope =
  let (width,height) = envelopeSize envelope
  in (-(x * width + envelopeLeft envelope)
     ,0)

alignYOffset : Float -> Envelope -> (Float,Float)
alignYOffset y envelope =
  let (width,height) = envelopeSize envelope
  in (0, -(y * height + envelopeTop envelope))

alignWith : (Envelope -> (Float, Float)) -> Bounded Form -> Bounded Form
alignWith alignOffset bform =
  let offset = alignOffset <| envelope bform
  in bform |> moveBy offset

alignList : (Envelope -> (Float, Float)) -> [Bounded Form] -> [Bounded Form]
alignList alignOffset bforms =
  let offset = alignOffset <| combineEnvelopes <| map envelope bforms
  in map (moveBy offset) bforms

originOf : Envelope -> Envelope -> (Float, Float)
originOf (Envelope originalL originalT _ _) (Envelope newL newT _ _) =
  (newL - originalL, newT - originalT)

lineBetween : LineStyle -> (Float,Float) -> (Float,Float) -> Bounded Form
lineBetween linestyle start end = bsegment start end $> traced linestyle
  
besidesList : [Bounded Form] -> [Bounded Form]
besidesList bforms = bforms |> attachAll1 (attachWith toRight)
                            |> alignList (alignXOffset 0.5)

besides : [Bounded Form] -> Bounded Form
besides bforms = overlayTopDown <| besidesList bforms

childrenConnections : ([Bounded Form] -> [Bounded Form])
                   -> (Bounded Form -> [Bounded Form] -> [Bounded Form])
                   -> ((Float,Float) -> (Float,Float) -> Bounded Form)
                   -> Bounded Form
                   -> [Bounded Form]
                   -> ([Bounded Form], [Bounded Form])
childrenConnections attachChildren attachChildrenToNode childConnection node children =
  case children of
    [] -> ([], [])
    _  ->
      let childrenPositioned = children |> attachChildren |> attachChildrenToNode node
          childrenOrigins    = zipWith originOf (map envelope children)
                                                (map envelope childrenPositioned)
          edges              = map (childConnection (0,0)) childrenOrigins
      in (edges, childrenPositioned)

connectChildren : Bounded Form -> [Bounded Form] -> Bounded Form
connectChildren node children =
  let getChildrenConnections =
        childrenConnections (alignList (alignXOffset 0.5)
                             . attachAll1 (attachWith (addOffset (5,0) toRight)))
                            (attachList (addOffset (0,30) toBottom))
                            (lineBetween defaultLine)
      (edges, childrenPositioned) = getChildrenConnections node children
  in overlayTopDown (node :: childrenPositioned ++ edges)




collageBounded : Bounded Form -> Element
collageBounded (Bounds envelope form) =
  let (w,h) = envelopeSize envelope
  in collage (round w) (round h)
             [form |> moveNative (-(w/2 + envelopeLeft envelope)
                                 ,-(h/2 + envelopeTop envelope))]

collageAt : (Float, Float) -> (Float, Float) -> Form -> Element
collageAt (xOrigin, yOrigin) (width, height) form =
  collage (floor width) (floor height)
          [form |> moveNative (-width/2 + xOrigin * width
                              ,-height/2 + yOrigin * height)]

fullscreenCollage : (Float, Float) -> Signal Form -> Signal Element
fullscreenCollage origin formsignal =
  lift2 (collageAt origin) (lift (onTuple toFloat) Window.dimensions) formsignal

fullscreenBounded : (Float, Float) -> Signal (Bounded Form) -> Signal Element
fullscreenBounded origin bformsignal =
  fullscreenCollage origin (lift form bformsignal)




drawBounds : Bounded Form -> Bounded Form
drawBounds bform = 
  let (w,h) = bsize bform
      boundingBox = brect w h $> outlined (solid black)
                              |> moveBy (w/2 + envelopeLeft (envelope bform)
                                        ,h/2 + envelopeTop (envelope bform))
      drawing = overlayTopDown [boundingBox, bform]
  in drawing

shapeOfEnvelope : Envelope -> Shape
shapeOfEnvelope env = uncurry rect (envelopeSize env)

envelopeOffset : Envelope -> (Float, Float)
envelopeOffset env =
  let (w,h) = envelopeSize env
  in (w/2 + envelopeLeft env, h/2 + envelopeTop env)

debugEnvelope : Bounded Form -> Bounded Form
debugEnvelope (Bounds envelope form) =
  let envelopeRect = shapeOfEnvelope envelope
                       |> outlined (solid (rgb 255 0 0))
                       |> moveNative (envelopeOffset envelope)
      originCircle = circle 3 |> outlined (solid red)
  in Bounds envelope (group [form, envelopeRect, originCircle])

nodebugEnvelope : Bounded Form -> Bounded Form
nodebugEnvelope bform = bform
