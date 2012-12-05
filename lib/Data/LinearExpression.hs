-- | Linear Expressions on Arbitrary @Field@ Types
module Data.LinearExpression ( LinearExpr(..), VarMapping, VariableName
                             , scalarMul, scalarAdd, add
                             , apply
                             , evaluate
                             , changeVar
                             )
                             where

import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as M
import Data.FieldTypes (Field(..))

-- | Variable Mapping
--
-- Maps a value to a @VariableName@
type VarMapping el = Map VariableName el
type VariableName = Text

-- | Linear Expression
--
-- /slope * variable + intercept/
data LinearExpr el = LinearExpr { leSlope     :: !el
                                , leVariable  :: !VariableName
                                , leIntercept :: !el
                                }
                   | ConstLinearExpr !el

-- | Mutiply a scalar to the linear expression.
scalarMul :: Field el => LinearExpr el -> el -> LinearExpr el
scalarMul le el =
    case le of
      ConstLinearExpr c -> ConstLinearExpr (c * el)
      LinearExpr s v i -> LinearExpr (s * el) v (i * el)

-- | Add two linear expressions.
add :: Field el => LinearExpr el -> LinearExpr el -> LinearExpr el
add l r =
    case (l, r) of
      (ConstLinearExpr cl, ConstLinearExpr cr) -> ConstLinearExpr (cl + cr)
      (LinearExpr sl vl il, ConstLinearExpr cr) -> LinearExpr sl vl (il + cr)
      (ConstLinearExpr cl, LinearExpr sr vr ir) -> LinearExpr sr vr (cl + ir)
      (LinearExpr sl vl il, LinearExpr sr vr ir) ->
          if vl == vr
             then LinearExpr (sl + sr) vl (il + ir)
             else error "add: cannot add linear expressions of different vars"

-- | Add a scalar to the linear expression.
scalarAdd :: Field el => LinearExpr el -> el -> LinearExpr el
scalarAdd le el =
    case le of
      ConstLinearExpr c -> ConstLinearExpr (c + el)
      LinearExpr s v i -> LinearExpr s v (i + el)

-- | Apply the @LinearExpression@ to a @LinearExpression@ resulting in a
-- @LinearExpression@.
apply :: Field el => el -> LinearExpr el -> el -> LinearExpr el
apply slope le intercept =
    case le of
      ConstLinearExpr c ->
          ConstLinearExpr $ slope * c + intercept
      LinearExpr slope' var intercept' ->
          -- a * (m * le + n) + b = (a*m) * le + (a*n + b)
          LinearExpr (slope * slope') var (slope*intercept' + intercept)

-- | Evaluate the linear expression using a @VarMapping@.
evaluate :: Field el => VarMapping el -> LinearExpr el -> Maybe el
evaluate varMap le =
    case le of
      ConstLinearExpr cle -> Just cle
      LinearExpr s v i ->
          do vVal <- M.lookup v varMap
             return $ s * vVal + i

-- | Rename the variable.
changeVar :: LinearExpr el -> VariableName -> LinearExpr el
changeVar le vnew =
    case le of
      cle@(ConstLinearExpr _) -> cle
      LinearExpr s _ i -> LinearExpr s vnew i

instance (Show el, Field el) => Show (LinearExpr el) where
    show le =
        case le of
          LinearExpr s v i -> show s ++ " * " ++ show v ++ " + " ++ show i
          ConstLinearExpr cle -> show cle
