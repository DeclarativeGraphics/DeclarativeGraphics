module Graphics.Declarative.Border where

import Graphics.Declarative.Transforms
import Linear hiding (translation, point)
import Data.Semigroup.Compat


data Border
  = Border { borderDistance :: V2 Double -> Double }
  | Empty

modifyDistFunc :: Border -> ((V2 Double -> Double) -> V2 Double -> Double) -> Border
modifyDistFunc Empty         modifyFunc = Empty
modifyDistFunc (Border func) modifyFunc = Border (modifyFunc func)

borderOffset :: Border -> V2 Double -> V2 Double
borderOffset Empty direction = V2 0 0
borderOffset border direction = borderDistance border direction *^ direction

borderSpanOnAxis :: Border -> V2 Double -> V2 Double
borderSpanOnAxis Empty axis = V2 0 0
borderSpanOnAxis border axis = forth - back
  where
    forth = borderOffset border axis
    back  = borderOffset border ((-1) *^ axis)

moveOriginTo :: V2 Double -> Border -> Border
moveOriginTo u border =
  modifyDistFunc border $ \f q -> f q - ((u ^/ (q `dot` q)) `dot` q)

instance Transformable Border where
  -- credit http://projects.haskell.org/diagrams/haddock/src/Diagrams-Core-Envelope.html#line-111
  transformBy matrix border = moveOriginTo ((-1) *^ translation matrix) $
      modifyDistFunc border $ \ f q ->
        let v' = signorm $ transposeTransform matrix q
            vi = inverseTransform matrix q
         in f v' / (v' `dot` vi)

instance Semigroup Border where
  a <> Empty = a
  Empty <> a = a
  (Border f1) <> (Border f2) = Border $ \q -> max (f1 q) (f2 q)

instance Monoid Border where
  mempty = Empty

padded :: Double -> Border -> Border
padded amount border = modifyDistFunc border $
  \f q -> amount / norm q + f q -- It's like adding a circle

getBoundingBox :: Border -> (V2 Double, V2 Double)
getBoundingBox border = (leftOff + topOff, rightOff + bottomOff)
  where
    leftOff   = borderOffset border left
    topOff    = borderOffset border up
    rightOff  = borderOffset border right
    bottomOff = borderOffset border down

size :: Border -> (Double, Double)
size border = (norm $ borderSpanOnAxis border right, norm $ borderSpanOnAxis border down)

point :: V2 Double -> Border
point pos = Border $ \q -> (q `dot` pos) / (q `dot` q)

circle :: Double -> Border
circle radius = Border $ \q -> radius / norm q

-- relative origin, within rectangle
rectangle :: (Double, Double) -> Double -> Double -> Border
rectangle (xorigin, yorigin) width height
  = fromBoundingBox (V2 (-width * xorigin)    (-height * yorigin),
                     V2 (width * (1-xorigin)) (height * (1-yorigin)))

fromBoundingBox :: (V2 Double, V2 Double) -> Border
fromBoundingBox (V2 l t, V2 r b) = mconcat $ map point corners
  where corners = [ V2 x y | x <- [l,r], y <- [t,b] ]
