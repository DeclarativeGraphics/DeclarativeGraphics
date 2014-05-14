module Bounded where

----- BOUNDED -----
data Bounded a = Bounds Envelope a

bcircle : Float -> Bounded Shape
bcircle radius = Bounds (circleEnvelope radius) (circle radius)

brect : Float -> Float -> Bounded Shape
brect width height = Bounds (rectEnvelope width height) (rect width height)

belement : Element -> Bounded Form
belement element = Bounds (elementEnvelope element) (toForm element)

bsegment : (Float,Float) -> (Float,Float) -> Bounded Path
bsegment start end = Bounds (segmentEnvelope start end) (segmentNative start end)

bempty : Float -> Float -> Bounded Form
bempty w h = Bounds (rectEnvelope w h) (group [])

envelope : Bounded a -> Envelope
envelope (Bounds envlp _) = envlp

form : Bounded a -> a
form (Bounds _ frm) = frm

bsize : Bounded a -> (Float,Float)
bsize (Bounds envelope _) = envelopeSize envelope

bscale : Float -> Bounded Form -> Bounded Form
bscale factor (Bounds envelope form) = Bounds (scaleEnvelope factor envelope)
                                              (scale factor form)


moveBy : (Float,Float) -> Bounded Form -> Bounded Form
moveBy pos (Bounds envelope form) = Bounds (moveEnvelope pos envelope)
                                           (moveNative pos form)

moveX : Float -> Bounded Form -> Bounded Form
moveX offset bform = moveBy (offset,0) bform

moveY : Float -> Bounded Form -> Bounded Form
moveY offset bform = moveBy (0,offset) bform


pad : Float -> Float -> Float -> Float -> Bounded a -> Bounded a
pad l t r b (Bounds envelope x) = Bounds (padEnvelope l t r b envelope) x

padAll : Float -> Bounded a -> Bounded a
padAll padding bform = pad padding padding padding padding bform

($>) : Bounded a -> (a -> b) -> Bounded b
(Bounds envelope a) $> f = Bounds envelope (f a)


overlayBottomUp : [Bounded Form] -> Bounded Form
overlayBottomUp boundedforms =
  let combinedForms = group (map form boundedforms)
      combinedEnvelopes = combineEnvelopes (map envelope boundedforms)
  in Bounds combinedEnvelopes combinedForms

overlayTopDown : [Bounded Form] -> Bounded Form
overlayTopDown bforms = overlayBottomUp (reverse bforms)

----- ENVELOPE -----
data Envelope = Envelope Float Float Float Float

circleEnvelope : Float -> Envelope
circleEnvelope radius = Envelope (-radius) (-radius) radius radius

rectEnvelope : Float -> Float -> Envelope
rectEnvelope width height = Envelope (-width/2) (-height/2) (width/2) (height/2)

elementEnvelope : Element -> Envelope
elementEnvelope element = let (width, height) = sizeOf element
                          in rectEnvelope (toFloat width) (toFloat height)

segmentEnvelope : (Float,Float) -> (Float,Float) -> Envelope
segmentEnvelope (x1,y1) (x2,y2) = Envelope (min x1 x2) (min y1 y2)
                                           (max x1 x2) (max y1 y2)

envelopeSize : Envelope -> (Float,Float)
envelopeSize (Envelope left top right bottom) = (right - left, bottom - top)

moveEnvelope : (Float,Float) -> Envelope -> Envelope
moveEnvelope (x,y) (Envelope left top right bottom)
  = Envelope (left + x) (top + y) (right + x) (bottom + y)

envelopeAtop : Envelope -> Envelope -> Envelope
envelopeAtop (Envelope left1 top1 right1 bottom1)
             (Envelope left2 top2 right2 bottom2)
  = Envelope (min left1 left2) (min top1 top2)
             (max right1 right2) (max bottom1 bottom2)

combineEnvelopes : [Envelope] -> Envelope
combineEnvelopes envelopes = foldl1 envelopeAtop envelopes

envelopeTop : Envelope -> Float
envelopeTop (Envelope _ top _ _) = top
envelopeLeft : Envelope -> Float
envelopeLeft (Envelope left _ _ _) = left
envelopeBottom : Envelope -> Float
envelopeBottom (Envelope _ _ _ bottom) = bottom
envelopeRight : Envelope -> Float
envelopeRight (Envelope _ _ right _) = right

padEnvelope : Float -> Float -> Float -> Float -> Envelope -> Envelope
padEnvelope l t r b (Envelope left top right bottom)
  = Envelope (left - l) (top - t) (right + r) (bottom + b)

scaleEnvelope : Float -> Envelope -> Envelope
scaleEnvelope factor (Envelope left top right bottom)
  = Envelope (factor * left) (factor * top) (factor * right) (factor * bottom)

---- STUFF ----
moveNative : (Float, Float) -> Form -> Form
moveNative = move . convertToNative

segmentNative : (Float, Float) -> (Float, Float) -> Path
segmentNative start end = segment (convertToNative start) (convertToNative end)

convertToNative : (Float, Float) -> (Float, Float)
convertToNative (x,y) = (x,-y)

onTuple : (a -> b) -> (a, a) -> (b, b)
onTuple f (x,y) = (f x, f y)

btext : String -> Bounded Form
btext = belement . centered . toText
