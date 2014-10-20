module Graphics.Declarative.Enveloped where

import Graphics.Declarative.Envelope
import Graphics.Declarative.Graphic

type EGraphic b = Enveloped (Graphic b)

data Enveloped a = Pack Envelope a


getEnvelope :: Enveloped a -> Envelope
getEnvelope (Pack e _) = e

setEnvelope :: Envelope -> Enveloped a -> Enveloped a
setEnvelope e = mapEnvelope $ const e

mapEnvelope :: (Envelope -> Envelope) -> (Enveloped a -> Enveloped a)
mapEnvelope f = \(Pack env a) -> Pack (f env) a

withBothEnvs :: (Envelope -> Envelope -> b) -> (Enveloped a -> Enveloped a -> b)
withBothEnvs f = \(Pack env1 g1) (Pack env2 g2) -> f env1 env2

liftEnv :: (Envelope -> Envelope) -> (a -> b) -> (Enveloped a -> Enveloped b)
liftEnv envelopeChanger graphicChanger = \(Pack env g) -> Pack (envelopeChanger env) (graphicChanger g)

liftEnv2 :: (Envelope -> Envelope -> Envelope) -> (a -> a -> a) -> (Enveloped a -> Enveloped a -> Enveloped a)
liftEnv2 envelopeCombiner graphicCombiner = \(Pack env1 g1) (Pack env2 g2)-> Pack (envelopeCombiner env1 env2) (graphicCombiner g1 g2)

noEnvelope :: Enveloped a -> Enveloped a
noEnvelope = setEnvelope emptyEnvelope


emptyEGraphic :: EGraphic b
emptyEGraphic = Pack emptyEnvelope Empty

move :: (Double, Double) -> EGraphic b -> EGraphic b
move dist = liftEnv (moveEnvelope dist) (moveGraphic dist)

atop :: EGraphic b -> EGraphic b -> EGraphic b
atop = liftEnv2 atopEnvelope atopGraphics


paddedWith :: (Double, Double, Double, Double) -> Enveloped a -> Enveloped a
paddedWith paddings = mapEnvelope (paddedWithEnvelope paddings)

paddedInDirs :: (Double, Double) -> Enveloped b -> Enveloped b
paddedInDirs (horizontally, vertically) = paddedWith (horizontally, vertically, vertically, horizontally)

padded :: Double -> Enveloped b -> Enveloped b
padded amount = paddedWith (amount, amount, amount, amount)


alignX :: Double -> EGraphic b -> EGraphic b
alignX relX g@(Pack (Envelope l _ r _) _) = move (relX * (l-r), 0) $ move (-l, 0) g

alignY :: Double -> EGraphic b -> EGraphic b
alignY relY g@(Pack (Envelope _ t _ b) _) = move (0, relY * (t-b)) $ move (0, -t) g

align :: (Double, Double) -> EGraphic b -> EGraphic b
align (xalign, yalign) = alignX xalign . alignY yalign


centeredX :: EGraphic b -> EGraphic b
centeredX = alignX 0.5

centeredY :: EGraphic b -> EGraphic b
centeredY = alignY 0.5

centered :: EGraphic b -> EGraphic b
centered = centeredX . centeredY


movedBesideWith :: (Envelope -> Envelope -> (Double, Double)) -> EGraphic b -> EGraphic b -> EGraphic b
movedBesideWith moveFunc refGraphic graphicToMove = move moveAmount graphicToMove
  where moveAmount = moveFunc (getEnvelope refGraphic) (getEnvelope graphicToMove)

moveAllBesideWith :: (Envelope -> Envelope -> (Double, Double)) -> [EGraphic b] -> [EGraphic b]
moveAllBesideWith moveFunc forms = scanl1 combine forms
  where combine form1 form2 = move ((withBothEnvs moveFunc) form1 form2) form2

groupBy :: (Envelope -> Envelope -> (Double, Double)) -> [EGraphic b] -> EGraphic b
groupBy _ [] = emptyEGraphic
groupBy moveFunc forms = foldr1 combine forms
  where combine form1 form2 = form1 `atop` move ((withBothEnvs moveFunc) form1 form2) form2


toRight :: Envelope -> Envelope -> (Double, Double)
toRight leftNeighbor rightNeighbor =
  (envToRight leftNeighbor - envToLeft rightNeighbor, 0)

toLeft :: Envelope -> Envelope -> (Double, Double)
toLeft rightNeighbor leftNeighbor =
  (envToLeft rightNeighbor - envToRight leftNeighbor, 0)

toTop :: Envelope -> Envelope -> (Double, Double)
toTop bottomNeighbor topNeighbor =
  (0, envToTop bottomNeighbor - envToBottom topNeighbor)

toBottom :: Envelope -> Envelope -> (Double, Double)
toBottom topNeighbor bottomNeighbor =
  (0, envToBottom topNeighbor - envToTop bottomNeighbor)


appendRight :: [EGraphic b] -> EGraphic b
appendRight = groupBy toRight

appendLeft :: [EGraphic b] -> EGraphic b
appendLeft = groupBy toLeft

appendUp :: [EGraphic b] -> EGraphic b
appendUp = groupBy toTop

appendDown :: [EGraphic b] -> EGraphic b
appendDown = groupBy toBottom


envTopBottom :: Enveloped a -> (Double, Double)
envTopBottom (Pack env _) = (envToTop env, envToBottom env)

envLeftRight :: Enveloped a -> (Double, Double)
envLeftRight (Pack env _) = (envToLeft env, envToRight env)

graphicHeight :: Enveloped a -> Double
graphicHeight = uncurry (flip (-)) . envTopBottom

graphicWidth :: Enveloped a -> Double
graphicWidth = uncurry (flip (-)) . envLeftRight
