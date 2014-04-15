module Graphics.Declarative.Combinators where

import Graphics.Declarative.Form
import Graphics.Declarative.Envelope

neighbored :: (Double, Double) -> Form -> Form -> Form
neighbored (distX, distY) formOnTop formBelow = formOnTop `atop` (moved (distX, distY) formBelow)

neighborDistanceX :: Form -> Form -> Double
neighborDistanceX leftForm rightForm = envToRight (fEnvelope leftForm) - envToLeft (fEnvelope rightForm)

neighborDistanceY :: Form -> Form -> Double
neighborDistanceY leftForm rightForm = envToBottom (fEnvelope leftForm) - envToTop (fEnvelope rightForm)

rightAttach :: Form -> Form -> Form
rightAttach leftForm rightForm = neighbored (neighborDistanceX leftForm rightForm, 0) leftForm rightForm

leftAttach :: Form -> Form -> Form
leftAttach leftForm rightForm = neighbored (-neighborDistanceX leftForm rightForm, 0) rightForm leftForm

downAttach :: Form -> Form -> Form
downAttach lowerForm upperForm = neighbored (0, neighborDistanceY lowerForm upperForm) lowerForm upperForm

upAttach :: Form -> Form -> Form
upAttach lowerForm upperForm = neighbored (0, -neighborDistanceY lowerForm upperForm) upperForm lowerForm

groupBy :: (Form -> Form -> Form) -> [Form] -> Form
groupBy _ [] = emptyForm
groupBy func ls = foldr1 func ls