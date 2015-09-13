module Graphics.Declarative.Bordered where

import Graphics.Declarative.Physical2D
import Graphics.Declarative.Border (Border(..))
import Graphics.Declarative.Graphic (Graphic(..))
import qualified Graphics.Declarative.Border as Border
import qualified Graphics.Declarative.Graphic as Graphic

import qualified Data.Vec2 as Vec2
import Data.Vec2 (Vec2)
import Utils

data Bordered a = Bordered Border a

instance Physical2D a => Physical2D (Bordered a) where
  move offset = liftBorder (move offset) (move offset)
  rotate angle = liftBorder (rotate angle) (rotate angle)
  scale factors = liftBorder (scale factors) (scale factors)
  atop = liftBorder2 atop atop
  empty = Bordered empty empty


noBorder :: graphic -> Bordered graphic
noBorder graphic = Bordered empty graphic

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
collapseBorder = setBorder empty

mapInner :: (a -> b) -> (Bordered a -> Bordered b)
mapInner f (Bordered border a) = Bordered border (f a)

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

padded :: Double -> Bordered a -> Bordered a
padded padding = onBorder (Border.padded padding) -- padding does not change the graphic

align :: Physical2D a => Vec2 -> Double -> Bordered a -> Bordered a
align axis alignment graphic = graphic |> moveOrigin $ alignDisplacement axis alignment graphic

alignDisplacement :: Vec2 -> Double -> Bordered a -> Vec2
alignDisplacement axis alignment graphic = back `Vec2.add` Vec2.scale alignment wholeSpan
  where
    back  = Border.borderOffset (getBorder graphic) (Vec2.negate axis)
    wholeSpan = Border.borderSpanOnAxis (getBorder graphic) axis

alignHoriz :: Physical2D a => Double -> Bordered a -> Bordered a
alignHoriz = align Vec2.right

alignVert :: Physical2D a => Double -> Bordered a -> Bordered a
alignVert = align Vec2.down

alignHV :: Physical2D a => (Double, Double) -> Bordered a -> Bordered a
alignHV (alignh, alignv) = alignHoriz alignh . alignVert alignv

centeredHoriz :: Physical2D a => Bordered a -> Bordered a
centeredHoriz = alignHoriz 0.5

centeredVert :: Physical2D a => Bordered a -> Bordered a
centeredVert = alignVert 0.5

centeredHV :: Physical2D a => Bordered a -> Bordered a
centeredHV = centeredHoriz . centeredVert


moveBesideBy :: Physical2D a => (Border -> Border -> (Double, Double)) -> Bordered a -> Bordered a -> Bordered a
moveBesideBy moveFunc refGraphic graphicToMove = move offset graphicToMove
  where offset = moveFunc (getBorder refGraphic) (getBorder graphicToMove)

moveAllBesideBy :: Physical2D a => (Border -> Border -> (Double, Double)) -> [Bordered a] -> [Bordered a]
moveAllBesideBy moveFunc forms = scanl1 combine forms
  where combine form1 form2 = move (onBorder2 moveFunc form1 form2) form2

groupBy :: Physical2D a => (Border -> Border -> (Double, Double)) -> [Bordered a] -> Bordered a
groupBy moveFunc forms = foldr1 combine forms
  where combine form1 form2 = form1 `atop` move (onBorder2 moveFunc form1 form2) form2

displacementTo :: Vec2 -> Border -> Border -> Vec2
displacementTo direction reference other
  = Border.borderOffset other (Vec2.negate direction) `Vec2.to` Border.borderOffset reference direction
  -- (Example: direction = Vec2.right:) Displace by the distance of the other's left Border + the distance to the reference's right border.
  -- Think: Make the other's left border be on the reference's right border.

append :: Physical2D a => Vec2 -> [Bordered a] -> Bordered a
append direction = groupBy (displacementTo direction)


graphicHeight :: Bordered a -> Double
graphicHeight graphic = Vec2.magnitude $ Border.borderSpanOnAxis (getBorder graphic) Vec2.down

graphicWidth :: Bordered a -> Double
graphicWidth graphic = Vec2.magnitude $ Border.borderSpanOnAxis (getBorder graphic) Vec2.right
