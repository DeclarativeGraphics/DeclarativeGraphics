module Graphics.Declarative.Bordered where

import Graphics.Declarative.Border (Border(..))
import Graphics.Declarative.Graphic (Graphic(..))
import qualified Graphics.Declarative.Border as Border
import qualified Graphics.Declarative.Graphic as Graphic

data Bordered a = Bordered Border a

noBorder :: graphic -> Bordered graphic
noBorder graphic = Bordered Border.empty graphic

bordered :: (Double, Double) -> Double -> Double -> graphic -> Bordered graphic
bordered origin width height graphic
  = Bordered (Border.rectangular origin width height) graphic


unborder :: Bordered a -> a
unborder (Bordered _ a) = a

getBorder :: Bordered a -> Border
getBorder (Bordered e _) = e

setBorder :: Border -> Bordered a -> Bordered a
setBorder e = onBorder $ const e

collapseBorder :: Bordered a -> Bordered a
collapseBorder = setBorder Border.empty

map :: (a -> b) -> (Bordered a -> Bordered b)
map f (Bordered border a) = Bordered border (f a)

onBorder :: (Border -> Border) -> (Bordered a -> Bordered a)
onBorder f (Bordered border a) = Bordered (f border) a

onBorder2 :: (Border -> Border -> b) -> (Bordered a -> Bordered a -> b)
onBorder2 f (Bordered border1 graphic1) (Bordered border2 graphic2)
  = f border1 border2

liftBorder :: (Border -> Border) -> (a -> b) -> (Bordered a -> Bordered b)
liftBorder onBorder onGraphic (Bordered border graphic)
  = Bordered (onBorder border) (onGraphic graphic)

liftBorder2 :: (Border -> Border -> Border) -> (a -> b -> c) -> (Bordered a -> Bordered b -> Bordered c)
liftBorder2 combineBorders combineGraphics (Bordered border1 graphic1) (Bordered border2 graphic2)
  = Bordered (combineBorders border1 border2) (combineGraphics graphic1 graphic2)


move :: (Double, Double) -> Bordered (Graphic b) -> Bordered (Graphic b)
move dist = liftBorder (Border.move dist) (Graphic.move dist)

atop :: Bordered (Graphic b) -> Bordered (Graphic b) -> Bordered (Graphic b)
atop = liftBorder2 Border.atop Graphic.atop


padded :: Double -> Bordered a -> Bordered a
padded padding = onBorder (Border.padded padding)

evenPadding padding = (padding, padding, padding, padding)
directionalPadding horizontal vertical = (horizontal, vertical, vertical, horizontal)


alignX :: Double -> Bordered (Graphic b) -> Bordered (Graphic b)
alignX relX g@(Bordered border _) = move (relX * (l-r), 0) $ move (-l, 0) g
  where l = Border.leftBorderOffset border
        r = Border.rightBorderOffset border

alignY :: Double -> Bordered (Graphic b) -> Bordered (Graphic b)
alignY relY g@(Bordered border _) = move (0, relY * (t-b)) $ move (0, -t) g
  where t = Border.topBorderOffset border
        b = Border.bottomBorderOffset border

align :: (Double, Double) -> Bordered (Graphic b) -> Bordered (Graphic b)
align (xalign, yalign) = alignX xalign . alignY yalign


centeredX :: Bordered (Graphic b) -> Bordered (Graphic b)
centeredX = alignX 0.5

centeredY :: Bordered (Graphic b) -> Bordered (Graphic b)
centeredY = alignY 0.5

centered :: Bordered (Graphic b) -> Bordered (Graphic b)
centered = centeredX . centeredY


moveBesideWith :: (Border -> Border -> (Double, Double)) -> Bordered (Graphic b) -> Bordered (Graphic b) -> Bordered (Graphic b)
moveBesideWith moveFunc refGraphic graphicToMove = move offset graphicToMove
  where offset = moveFunc (getBorder refGraphic) (getBorder graphicToMove)

moveAllBesideWith :: (Border -> Border -> (Double, Double)) -> [Bordered (Graphic b)] -> [Bordered (Graphic b)]
moveAllBesideWith moveFunc forms = scanl1 combine forms
  where combine form1 form2 = move (onBorder2 moveFunc form1 form2) form2

groupBy :: (Border -> Border -> (Double, Double)) -> [Bordered (Graphic b)] -> Bordered (Graphic b)
groupBy moveFunc forms = foldr1 combine forms
  where combine form1 form2 = form1 `atop` move (onBorder2 moveFunc form1 form2) form2


toRight :: Border -> Border -> (Double, Double)
toRight leftNeighbor rightNeighbor =
  (Border.rightBorderOffset leftNeighbor - Border.leftBorderOffset rightNeighbor, 0)

toLeft :: Border -> Border -> (Double, Double)
toLeft rightNeighbor leftNeighbor =
  (Border.leftBorderOffset rightNeighbor - Border.rightBorderOffset leftNeighbor, 0)

toTop :: Border -> Border -> (Double, Double)
toTop bottomNeighbor topNeighbor =
  (0, Border.topBorderOffset bottomNeighbor - Border.bottomBorderOffset topNeighbor)

toBottom :: Border -> Border -> (Double, Double)
toBottom topNeighbor bottomNeighbor =
  (0, Border.bottomBorderOffset topNeighbor - Border.topBorderOffset bottomNeighbor)


appendRight :: [Bordered (Graphic b)] -> Bordered (Graphic b)
appendRight = groupBy toRight

appendLeft :: [Bordered (Graphic b)] -> Bordered (Graphic b)
appendLeft = groupBy toLeft

appendUp :: [Bordered (Graphic b)] -> Bordered (Graphic b)
appendUp = groupBy toTop

appendDown :: [Bordered (Graphic b)] -> Bordered (Graphic b)
appendDown = groupBy toBottom


topBottomBorderOffsets :: Bordered a -> (Double, Double)
topBottomBorderOffsets (Bordered border _) = (Border.topBorderOffset border, Border.bottomBorderOffset border)

leftRightBorderOffsets :: Bordered a -> (Double, Double)
leftRightBorderOffsets (Bordered border _) = (Border.leftBorderOffset border, Border.rightBorderOffset border)

graphicHeight :: Bordered a -> Double
graphicHeight = uncurry (flip (-)) . topBottomBorderOffsets

graphicWidth :: Bordered a -> Double
graphicWidth = uncurry (flip (-)) . leftRightBorderOffsets
