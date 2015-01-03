module Graphics.Declarative.Bordered where

import Graphics.Declarative.Border (Border(..))
import Graphics.Declarative.Graphic (Graphic(..))
import qualified Graphics.Declarative.Border as Border
import qualified Graphics.Declarative.Graphic as Graphic

import Data.Vec2 as Vec2
import Utils

data Bordered a = Bordered Border a

noBorder :: graphic -> Bordered graphic
noBorder graphic = Bordered Border.empty graphic

bordered :: (Double, Double) -> Double -> Double -> graphic -> Bordered graphic
bordered origin width height graphic
  = Bordered (Border.rectangle origin width height) graphic


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

align :: Vec2 -> Double -> Bordered (Graphic b) -> Bordered (Graphic b)
align axis alignment graphic
  = graphic |> move back
            |> move (scale alignment wholeSpan)
  where
    back  = Border.borderOffset (getBorder graphic) (Vec2.negate axis)
    wholeSpan = Border.borderSpanOnAxis (getBorder graphic) axis

alignHoriz :: Double -> Bordered (Graphic b) -> Bordered (Graphic b)
alignHoriz = align Vec2.right

alignVert :: Double -> Bordered (Graphic b) -> Bordered (Graphic b)
alignVert = align Vec2.down

alignHV :: (Double, Double) -> Bordered (Graphic b) -> Bordered (Graphic b)
alignHV (alignh, alignv) = alignHoriz alignh . alignVert alignv

centeredHoriz :: Bordered (Graphic b) -> Bordered (Graphic b)
centeredHoriz = alignHoriz 0.5

centeredVert :: Bordered (Graphic b) -> Bordered (Graphic b)
centeredVert = alignVert 0.5

centeredHV :: Bordered (Graphic b) -> Bordered (Graphic b)
centeredHV = centeredHoriz . centeredVert


moveBesideWith :: (Border -> Border -> (Double, Double)) -> Bordered (Graphic b) -> Bordered (Graphic b) -> Bordered (Graphic b)
moveBesideWith moveFunc refGraphic graphicToMove = move offset graphicToMove
  where offset = moveFunc (getBorder refGraphic) (getBorder graphicToMove)

moveAllBesideWith :: (Border -> Border -> (Double, Double)) -> [Bordered (Graphic b)] -> [Bordered (Graphic b)]
moveAllBesideWith moveFunc forms = scanl1 combine forms
  where combine form1 form2 = move (onBorder2 moveFunc form1 form2) form2

groupBy :: (Border -> Border -> (Double, Double)) -> [Bordered (Graphic b)] -> Bordered (Graphic b)
groupBy moveFunc forms = foldr1 combine forms
  where combine form1 form2 = form1 `atop` move (onBorder2 moveFunc form1 form2) form2

displacementTo :: Vec2 -> Border -> Border -> Vec2
displacementTo direction reference other
  = Border.borderOffset reference direction `Vec2.to` Border.borderOffset other (Vec2.negate direction)


append :: Vec2 -> [Bordered (Graphic b)] -> Bordered (Graphic b)
append direction = groupBy (displacementTo direction)

graphicHeight :: Bordered a -> Double
graphicHeight graphic = Vec2.magnitude $ Border.borderSpanOnAxis (getBorder graphic) Vec2.down

graphicWidth :: Bordered a -> Double
graphicWidth graphic = Vec2.magnitude $ Border.borderSpanOnAxis (getBorder graphic) Vec2.right
