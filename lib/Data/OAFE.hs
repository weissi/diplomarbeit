--------------------------------------------------------------------------------
--  This file is part of diplomarbeit ("Diplomarbeit Johannes Weiß").         --
--                                                                            --
--  diplomarbeit is free software: you can redistribute it and/or modify      --
--  it under the terms of the GNU General Public License as published by      --
--  the Free Software Foundation, either version 3 of the License, or         --
--  (at your option) any later version.                                       --
--                                                                            --
--  diplomarbeit is distributed in the hope that it will be useful,           --
--  but WITHOUT ANY WARRANTY; without even the implied warranty of            --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             --
--  GNU General Public License for more details.                              --
--                                                                            --
--  You should have received a copy of the GNU General Public License         --
--  along with diplomarbeit.  If not, see <http://www.gnu.org/licenses/>.     --
--                                                                            --
--  Copyright 2012, Johannes Weiß                                             --
--------------------------------------------------------------------------------

-- | OAFE Types and Functions.
module Data.OAFE
    ( OAFEEvaluation
    , OAFEEvaluationRequest
    , OAFEEvaluationResponse
    , OAFEConfiguration
    , OAFEReference(..)
    , processOAFEEvaluationRequest
    ) where

-- # SITE PACKAGES
import Data.HashMap.Strict (HashMap)
import Data.Vector (Vector)

import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

-- # LOCAL
import Data.FieldTypes (Field)
import Data.LinearExpression (VariableName)

-- | The result of all past OAFE evaluations: A map from 'VariableName' to
-- the evaluated values (as 'Vector').
type OAFEEvaluation el    = HashMap VariableName (Vector el)

-- | Request to evaluate one OAFE giving a 'VariableName' and a value.
type OAFEEvaluationRequest el = (VariableName, el)


-- | Response of one OAFE evaluation (the 'VariableName' that has been evaluated
-- with and the values (as 'Vector')).
type OAFEEvaluationResponse el = (VariableName, Vector el)

-- | The OAFE configuration for all OAFEs.
type OAFEConfiguration el = HashMap VariableName (Vector (el, el))

-- | A reference to one OAFE and a specific row in the result vector.
data OAFEReference = OAFERef !VariableName !Int deriving Show

-- | Process one 'OAFEEvaluationRequest'.
-- This is used by the @Token@. A implementation of the /David & Goliath/
-- protocol would replace this by the actual sub-protocol call.
processOAFEEvaluationRequest :: Field el
                             => OAFEConfiguration el
                             -> OAFEEvaluationRequest el
                             -> OAFEEvaluationResponse el
processOAFEEvaluationRequest oac (var, val) =
    case HM.lookup var oac of
      Nothing -> (var, V.empty)
      Just xs -> (var, V.map (\(s, i) -> s * val + i) xs)
