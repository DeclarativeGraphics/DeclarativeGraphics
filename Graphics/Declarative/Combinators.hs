module Graphics.Declarative.Combinators where

import Graphics.Declarative.Form
import Graphics.Declarative.Envelope

liftToForm2 :: (Envelope -> Envelope -> a) -> (Form -> Form -> a)
liftToForm2 func = \form1 form2 -> func (fEnvelope form1) (fEnvelope form2)

movedBesideWith :: (Envelope -> Envelope -> (Double, Double)) -> Form -> Form -> Form
movedBesideWith moveFunc referenceForm formToMove = moved move formToMove
  where move = moveFunc (fEnvelope referenceForm) (fEnvelope formToMove)

moveAllBesideWith :: (Envelope -> Envelope -> (Double, Double)) -> [Form] -> [Form]
moveAllBesideWith moveFunc forms = scanl1 combine forms
  where combine form1 form2 = moved ((liftToForm2 moveFunc) form1 form2) form2

groupBy :: (Envelope -> Envelope -> (Double, Double)) -> [Form] -> Form
groupBy _ [] = emptyForm
groupBy moveFunc forms = foldr1 combine forms
  where combine form1 form2 = form1 `atop` moved ((liftToForm2 moveFunc) form1 form2) form2

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

