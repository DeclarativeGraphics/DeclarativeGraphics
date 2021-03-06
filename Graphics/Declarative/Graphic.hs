module Graphics.Declarative.Graphic where

import Graphics.Declarative.Transforms
import Linear

type TransformFunc backend = M33 Double -> backend -> backend
type AtopFunc backend = backend -> backend -> backend

newtype Graphic backend = Graphic (TransformFunc backend -> AtopFunc backend -> backend -> backend)

renderGraphic :: TransformFunc backend -> AtopFunc backend -> backend -> Graphic backend -> backend
renderGraphic transform atop empty (Graphic renderer) = renderer transform atop empty

primitive :: backend -> Graphic backend
primitive prim = Graphic $ \ transform atop empty -> prim

instance Transformable (Graphic a) where
  transformBy mat graphic = Graphic $ \ transform atop empty -> transform mat (renderGraphic transform atop empty graphic)

instance Semigroup (Graphic a) where
  upper <> lower = Graphic $ \ transform atop empty -> atop (renderGraphic transform atop empty upper) (renderGraphic transform atop empty lower)

instance Monoid (Graphic a) where
  mempty = Graphic $ \ transform atop empty -> empty
