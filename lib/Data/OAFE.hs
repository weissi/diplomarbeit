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

type OAFEEvaluation el    = HashMap VariableName (Vector el)
type OAFEEvaluationRequest el = (VariableName, el)
type OAFEEvaluationResponse el = (VariableName, Vector el)
type OAFEConfiguration el = HashMap VariableName (Vector (el, el))
data OAFEReference = OAFERef !VariableName !Int deriving Show

-- | Process one @OAFEEvaluationRequest@.
processOAFEEvaluationRequest :: Field el
                             => OAFEConfiguration el
                             -> OAFEEvaluationRequest el
                             -> OAFEEvaluationResponse el
processOAFEEvaluationRequest oac (var, val) =
    case HM.lookup var oac of
      Nothing -> (var, V.empty)
      Just xs -> (var, V.map (\(s, i) -> s * val + i) xs)
