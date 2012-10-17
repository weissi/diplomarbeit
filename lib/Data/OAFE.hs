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
