module Graphics.Declarative.Bordered where

import Graphics.Declarative.Transforms
import Graphics.Declarative.Border (Border(..))
import Graphics.Declarative.Graphic (Graphic(..))
import qualified Graphics.Declarative.Border as Border
import qualified Graphics.Declarative.Graphic as Graphic

import Linear
import Data.Function ((&))

data Bordered a = Bordered Border a

class HasBorder a where
  getBorder :: a -> Border

instance HasBorder (Bordered a) where
  getBorder = getBorderedBorder

instance Transformable a => Transformable (Bordered a) where
  transformBy mat = liftBorder (transformBy mat) (transformBy mat)

instance Semigroup a => Semigroup (Bordered a) where
  (<>) = liftBorder2 (<>) (<>)

instance Monoid a => Monoid (Bordered a) where
  mempty = noBorder mempty

noBorder :: graphic -> Bordered graphic
noBorder graphic = Bordered mempty graphic

bordered :: (Double, Double) -> Double -> Double -> graphic -> Bordered graphic
bordered origin width height graphic
  = Bordered (Border.rectangle origin width height) graphic


unborder :: Bordered a -> a
unborder (Bordered _ a) = a

getBorderedBorder :: Bordered a -> Border
getBorderedBorder (Bordered e _) = e

setBorder :: Border -> Bordered a -> Bordered a
setBorder e = onBorder $ const e

collapseBorder :: Bordered a -> Bordered a
collapseBorder = setBorder mempty

mapInner :: (a -> b) -> (Bordered a -> Bordered b)
mapInner f (Bordered border a) = Bordered border (f a)

onBorder :: (Border -> Border) -> (Bordered a -> Bordered a)
onBorder f (Bordered border a) = Bordered (f border) a

onBorder2 :: HasBorder a => (Border -> Border -> b) -> (a -> a -> b)
onBorder2 f hasBorder1 hasBorder2 = f (getBorder hasBorder1) (getBorder hasBorder2)

liftBorder :: (Border -> Border) -> (a -> b) -> (Bordered a -> Bordered b)
liftBorder onBorder onGraphic (Bordered border graphic)
  = Bordered (onBorder border) (onGraphic graphic)

liftBorder2 :: (Border -> Border -> Border) -> (a -> b -> c) -> (Bordered a -> Bordered b -> Bordered c)
liftBorder2 combineBorders combineGraphics (Bordered border1 graphic1) (Bordered border2 graphic2)
  = Bordered (combineBorders border1 border2) (combineGraphics graphic1 graphic2)

padded :: Double -> Bordered a -> Bordered a
padded padding = onBorder (Border.padded padding) -- padding does not change the graphic

align :: (HasBorder a, Transformable a) => V2 Double -> Double -> a -> a
align axis alignment graphic = graphic & moveOrigin (alignDisplacement axis alignment graphic)

alignDisplacement :: HasBorder a => V2 Double -> Double -> a -> V2 Double
alignDisplacement axis alignment graphic = back + alignment *^ wholeSpan
  where
    back  = Border.borderOffset (getBorder graphic) ((-1) *^ axis)
    wholeSpan = Border.borderSpanOnAxis (getBorder graphic) axis

alignHoriz :: (HasBorder a, Transformable a) => Double -> a -> a
alignHoriz = align right

alignVert :: (HasBorder a, Transformable a) => Double -> a -> a
alignVert = align down

alignHV :: (HasBorder a, Transformable a) => (Double, Double) -> a -> a
alignHV (alignh, alignv) = alignHoriz alignh . alignVert alignv

centeredHoriz :: (HasBorder a, Transformable a) => a -> a
centeredHoriz = alignHoriz 0.5

centeredVert :: (HasBorder a, Transformable a) => a -> a
centeredVert = alignVert 0.5

centeredHV :: (HasBorder a, Transformable a) => a -> a
centeredHV = centeredHoriz . centeredVert


moveBesideBy :: (HasBorder ref, HasBorder a, Transformable a) => (Border -> Border -> V2 Double) -> ref -> a -> a
moveBesideBy moveFunc refGraphic graphicToMove = move offset graphicToMove
  where offset = moveFunc (getBorder refGraphic) (getBorder graphicToMove)

moveAllBesideBy :: (HasBorder a, Transformable a) => (Border -> Border -> V2 Double) -> [a] -> [a]
moveAllBesideBy moveFunc = scanl1 combine -- TODO: Don't use partial functions
  where combine form1 form2 = move (onBorder2 moveFunc form1 form2) form2

groupBy :: (HasBorder a, Transformable a, Monoid a) => (Border -> Border -> V2 Double) -> [a] -> a
groupBy moveFunc = foldr combine mempty
  where combine form1 form2 = form1 <> move (onBorder2 moveFunc form1 form2) form2

displacementTo :: V2 Double -> Border -> Border -> V2 Double
displacementTo direction reference other
  = Border.borderOffset reference direction - Border.borderOffset other ((-1) *^ direction)
  -- (Example: direction = right:) Displace by the distance of the other's left Border + the distance to the reference's right border.
  -- Think: Make the other's left border be on the reference's right border.

appendTo :: (HasBorder a, Transformable a, Monoid a) => V2 Double -> [a] -> a
appendTo direction = groupBy (displacementTo direction)

placedBesidesTo :: (HasBorder a, Transformable a) => V2 Double -> [a] -> [a]
placedBesidesTo direction = moveAllBesideBy (displacementTo direction)


graphicHeight :: HasBorder a => a -> Double
graphicHeight graphic = norm $ Border.borderSpanOnAxis (getBorder graphic) down

graphicWidth :: HasBorder a => a -> Double
graphicWidth graphic = norm $ Border.borderSpanOnAxis (getBorder graphic) right
