module Graphics.Declarative.Framed where

import Graphics.Declarative.Frame (Frame(..))
import Graphics.Declarative.Graphic (Graphic(..))
import qualified Graphics.Declarative.Frame as Frame
import qualified Graphics.Declarative.Graphic as Graphic

data Framed a = Framed Frame a

noFrame :: graphic -> Framed graphic
noFrame graphic = Framed Frame.empty graphic

framed origin width height backend
  = Framed (Frame.frame origin width height) backend


unframe :: Framed a -> a
unframe (Framed _ a) = a

getFrame :: Framed a -> Frame
getFrame (Framed e _) = e

setFrame :: Frame -> Framed a -> Framed a
setFrame e = onFrame $ const e

collapseFrame :: Framed a -> Framed a
collapseFrame = setFrame Frame.empty

map :: (a -> b) -> (Framed a -> Framed b)
map f (Framed frame a) = Framed frame (f a)

onFrame :: (Frame -> Frame) -> (Framed a -> Framed a)
onFrame f (Framed frame a) = Framed (f frame) a

onFrame2 :: (Frame -> Frame -> b) -> (Framed a -> Framed a -> b)
onFrame2 f (Framed frame1 graphic1) (Framed frame2 graphic2)
  = f frame1 frame2

liftFrame :: (Frame -> Frame) -> (a -> b) -> (Framed a -> Framed b)
liftFrame onFrame onGraphic (Framed frame graphic)
  = Framed (onFrame frame) (onGraphic graphic)

liftFrame2 :: (Frame -> Frame -> Frame) -> (a -> b -> c) -> (Framed a -> Framed b -> Framed c)
liftFrame2 combineFrames combineGraphics (Framed frame1 graphic1) (Framed frame2 graphic2)
  = Framed (combineFrames frame1 frame2) (combineGraphics graphic1 graphic2)


move :: (Double, Double) -> Framed (Graphic b) -> Framed (Graphic b)
move dist = liftFrame (Frame.move dist) (Graphic.move dist)

atop :: Framed (Graphic b) -> Framed (Graphic b) -> Framed (Graphic b)
atop = liftFrame2 Frame.atop Graphic.atop


padded :: (Double, Double, Double, Double) -> Framed a -> Framed a
padded paddings = onFrame (Frame.padded paddings)

evenPadding padding = (padding, padding, padding, padding)
directionalPadding horizontal vertical = (horizontal, vertical, vertical, horizontal)


alignX :: Double -> Framed (Graphic b) -> Framed (Graphic b)
alignX relX g@(Framed (Frame l _ r _) _) = move (relX * (l-r), 0) $ move (-l, 0) g

alignY :: Double -> Framed (Graphic b) -> Framed (Graphic b)
alignY relY g@(Framed (Frame _ t _ b) _) = move (0, relY * (t-b)) $ move (0, -t) g

align :: (Double, Double) -> Framed (Graphic b) -> Framed (Graphic b)
align (xalign, yalign) = alignX xalign . alignY yalign


centeredX :: Framed (Graphic b) -> Framed (Graphic b)
centeredX = alignX 0.5

centeredY :: Framed (Graphic b) -> Framed (Graphic b)
centeredY = alignY 0.5

centered :: Framed (Graphic b) -> Framed (Graphic b)
centered = centeredX . centeredY


moveBesideWith :: (Frame -> Frame -> (Double, Double)) -> Framed (Graphic b) -> Framed (Graphic b) -> Framed (Graphic b)
moveBesideWith moveFunc refGraphic graphicToMove = move offset graphicToMove
  where offset = moveFunc (getFrame refGraphic) (getFrame graphicToMove)

moveAllBesideWith :: (Frame -> Frame -> (Double, Double)) -> [Framed (Graphic b)] -> [Framed (Graphic b)]
moveAllBesideWith moveFunc forms = scanl1 combine forms
  where combine form1 form2 = move (onFrame2 moveFunc form1 form2) form2

groupBy :: (Frame -> Frame -> (Double, Double)) -> [Framed (Graphic b)] -> Framed (Graphic b)
groupBy moveFunc forms = foldr1 combine forms
  where combine form1 form2 = form1 `atop` move (onFrame2 moveFunc form1 form2) form2


toRight :: Frame -> Frame -> (Double, Double)
toRight leftNeighbor rightNeighbor =
  (Frame.rightBorderOffset leftNeighbor - Frame.leftBorderOffset rightNeighbor, 0)

toLeft :: Frame -> Frame -> (Double, Double)
toLeft rightNeighbor leftNeighbor =
  (Frame.leftBorderOffset rightNeighbor - Frame.rightBorderOffset leftNeighbor, 0)

toTop :: Frame -> Frame -> (Double, Double)
toTop bottomNeighbor topNeighbor =
  (0, Frame.topBorderOffset bottomNeighbor - Frame.bottomBorderOffset topNeighbor)

toBottom :: Frame -> Frame -> (Double, Double)
toBottom topNeighbor bottomNeighbor =
  (0, Frame.bottomBorderOffset topNeighbor - Frame.topBorderOffset bottomNeighbor)


appendRight :: [Framed (Graphic b)] -> Framed (Graphic b)
appendRight = groupBy toRight

appendLeft :: [Framed (Graphic b)] -> Framed (Graphic b)
appendLeft = groupBy toLeft

appendUp :: [Framed (Graphic b)] -> Framed (Graphic b)
appendUp = groupBy toTop

appendDown :: [Framed (Graphic b)] -> Framed (Graphic b)
appendDown = groupBy toBottom


topBottomBorderOffsets :: Framed a -> (Double, Double)
topBottomBorderOffsets (Framed frame _) = (Frame.topBorderOffset frame, Frame.bottomBorderOffset frame)

leftRightBorderOffsets :: Framed a -> (Double, Double)
leftRightBorderOffsets (Framed frame _) = (Frame.leftBorderOffset frame, Frame.rightBorderOffset frame)

graphicHeight :: Framed a -> Double
graphicHeight = uncurry (flip (-)) . topBottomBorderOffsets

graphicWidth :: Framed a -> Double
graphicWidth = uncurry (flip (-)) . leftRightBorderOffsets
