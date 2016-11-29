module Graphics.Declarative.Bordered where

import Graphics.Declarative.Classes
import Graphics.Declarative.Border (Border(..))
import Graphics.Declarative.Graphic (Graphic(..))
import qualified Graphics.Declarative.Border as Border
import qualified Graphics.Declarative.Graphic as Graphic

import Linear
import Data.Function ((&))

data Bordered a = Bordered Border a

instance Transformable a => Transformable (Bordered a) where
  transformBy mat = liftBorder (transformBy mat) (transformBy mat)

instance Combinable a => Combinable (Bordered a) where
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

align :: Transformable a => V2 Double -> Double -> Bordered a -> Bordered a
align axis alignment graphic = graphic & moveOrigin (alignDisplacement axis alignment graphic)

alignDisplacement :: V2 Double -> Double -> Bordered a -> V2 Double
alignDisplacement axis alignment graphic = back + alignment *^ wholeSpan
  where
    back  = Border.borderOffset (getBorder graphic) ((-1) *^ axis)
    wholeSpan = Border.borderSpanOnAxis (getBorder graphic) axis

alignHoriz :: Transformable a => Double -> Bordered a -> Bordered a
alignHoriz = align right

alignVert :: Transformable a => Double -> Bordered a -> Bordered a
alignVert = align down

alignHV :: Transformable a => (Double, Double) -> Bordered a -> Bordered a
alignHV (alignh, alignv) = alignHoriz alignh . alignVert alignv

centeredHoriz :: Transformable a => Bordered a -> Bordered a
centeredHoriz = alignHoriz 0.5

centeredVert :: Transformable a => Bordered a -> Bordered a
centeredVert = alignVert 0.5

centeredHV :: Transformable a => Bordered a -> Bordered a
centeredHV = centeredHoriz . centeredVert


moveBesideBy :: Transformable a => (Border -> Border -> V2 Double) -> Bordered a -> Bordered a -> Bordered a
moveBesideBy moveFunc refGraphic graphicToMove = move offset graphicToMove
  where offset = moveFunc (getBorder refGraphic) (getBorder graphicToMove)

moveAllBesideBy :: Transformable a => (Border -> Border -> V2 Double) -> [Bordered a] -> [Bordered a]
moveAllBesideBy moveFunc forms = scanl1 combine forms
  where combine form1 form2 = move (onBorder2 moveFunc form1 form2) form2

groupBy :: (Transformable a, Combinable a) => (Border -> Border -> V2 Double) -> [Bordered a] -> Bordered a
groupBy moveFunc forms = foldr1 combine forms
  where combine form1 form2 = form1 `atop` move (onBorder2 moveFunc form1 form2) form2

displacementTo :: V2 Double -> Border -> Border -> V2 Double
displacementTo direction reference other
  = Border.borderOffset reference direction - Border.borderOffset other ((-1) *^ direction)
  -- (Example: direction = right:) Displace by the distance of the other's left Border + the distance to the reference's right border.
  -- Think: Make the other's left border be on the reference's right border.

appendTo :: (Transformable a, Combinable a) => V2 Double -> [Bordered a] -> Bordered a
appendTo direction = groupBy (displacementTo direction)

placedBesidesTo :: Transformable a => V2 Double -> [Bordered a] -> [Bordered a]
placedBesidesTo direction = moveAllBesideBy (displacementTo direction)


graphicHeight :: Bordered a -> Double
graphicHeight graphic = norm $ Border.borderSpanOnAxis (getBorder graphic) down

graphicWidth :: Bordered a -> Double
graphicWidth graphic = norm $ Border.borderSpanOnAxis (getBorder graphic) right
