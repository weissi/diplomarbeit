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

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-} -- for 'failure' library

-- | This module provides various RAE evaluation possibilities.
module Data.RAE.Evaluation
    ( -- * Public API
      evalRAE
    , RAEEvaluationFailure(..)
      -- * Important Special Variable Names
    , _SPECIAL_VAR_OUT_LEFT_, _SPECIAL_VAR_OUT_RIGHT_
    , _SPECIAL_VAR_PRE_OUT_LEFT_, _SPECIAL_VAR_PRE_OUT_RIGHT_
    , _SPECIAL_VAR_ADDED_PRE_OUT_
    , _SPECIAL_DUAL_VAR_OUT_, _SPECIAL_DUAL_VAR_PRE_OUT_
      -- * Direct Evaluation of @RAC@s and @DRAC@s (for tests/benchmarks)
    , runRAC, runDRAC
    ) where

-- # STANDARD LIBRARY
import Data.List (foldl')
import Control.Monad (foldM, liftM)
import Control.Monad.Trans (lift)
import Control.Monad.State.Strict ( State, StateT, get, put
                                  , execStateT, execState
                                  )
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

-- # SITE PACKAGES
import Control.Failure (Failure, failure)
import Data.Vector (Vector)
import qualified Data.DList as DL

-- # LOCAL
import Data.FieldTypes (Field(..))
import Data.LinearExpression (VariableName, VarMapping)
import Data.OAFE (OAFEConfiguration, OAFEEvaluation, OAFEReference(..))
import Data.RAE.Decoder (decodeDRAE)
import Data.RAE.Types ( DRAC, DRACFragment, RACFragment
                      , RAE(..), RAC, DualVarName(..), genDualVarName
                      , RadicalTuple(..), RadicalSingleton(..)
                      )


-- | Indicates what went wrong when the evaluation fails.
data RAEEvaluationFailure =  UnknownFailure String String
                           | UnknownVariable VariableName String
                           | IndexOutOfBounds VariableName Int String
                           deriving Show

-- | The monad used to execute a @RAC@.
type RunRACStateMonad m el = StateT (OAFEEvaluation el) m

-- | The monad used to execute a @DRAC@.
type RunDRACStateMonad el = State (VarMapping el)

-- | Directly evaluate a @DRAC@.
--
-- This is usually only in use in tests and benchmarks.
runDRAC :: forall el. (Field el)
        => VarMapping el
        -> DRAC el
        -> (Maybe el, VarMapping el)
runDRAC initialVarMap drac =
  let outVarMap :: VarMapping el
      outVarMap = execState (mapM_ execDRACFragment (DL.toList drac))
                            initialVarMap
      out =
          case ( M.lookup _SPECIAL_VAR_OUT_LEFT_ outVarMap
               , M.lookup _SPECIAL_VAR_OUT_RIGHT_ outVarMap
               ) of
            (Just l, Just r) ->
               if l == r then Just l else error "the impossible happened"
            _ -> Nothing
   in (out, outVarMap)

-- | Directly evaluate a @RAC@.
--
-- This is usually only in use in tests and benchmarks.
runRAC :: forall el. (Field el)
       => RAC el
       -> OAFEConfiguration el
       -> VarMapping el
       -> Either String el
runRAC racFrag oac vars =
    case doIt of
      Left err -> Left $ show (err :: RAEEvaluationFailure)
      Right val -> Right $ V.head val
    where doIt =
              do let oac' = oac `HM.union`
                            HM.fromList
                                [ (_SPECIAL_VAR_OUT_LEFT_, oneZeroV)
                                , (_SPECIAL_VAR_OUT_RIGHT_, oneZeroV)
                                , (_SPECIAL_VAR_PRE_OUT_LEFT_, oneZeroV)
                                , (_SPECIAL_VAR_OUT_RIGHT_, oneZeroV)
                                ]
                 oae <- initiallyEvaluateOAFE oac' vars
                 out <- execStateT (mapM_ (execRACFragment oac') racFrag) oae
                 case HM.lookup _SPECIAL_VAR_OUT_LEFT_ out of
                   Just val -> return val
                   Nothing ->
                       failure $ UnknownFailure "output not calculated" "runRAC"

-- | Initially evaluate an OAFE.
--
-- Helper method for @runRAC@
initiallyEvaluateOAFE :: (Failure RAEEvaluationFailure f, Field el)
                      => OAFEConfiguration el
                      -> VarMapping el
                      -> f (OAFEEvaluation el)
initiallyEvaluateOAFE oac vars =
    foldM (evaluateOAFE oac) HM.empty $ M.toList vars

-- | Partially evaluate an OAFE.
--
-- Helper function.
evaluateOAFE :: (Failure RAEEvaluationFailure f, Field el)
             => OAFEConfiguration el
             -> OAFEEvaluation el
             -> (VariableName, el)
             -> f (OAFEEvaluation el)
evaluateOAFE oac curEval (var, val) =
    case HM.lookup var oac of
      Nothing -> return curEval
          {-
          failure $
              UnknownFailure ("Variable `"++var++"' not found in OAFE config")
                             "evaluateOAFE"
          -}
      Just xs ->
          return $! HM.insert var (V.map (\(s, i) -> s * val + i) xs) curEval

evaluateRAE' :: forall f. forall el.
                 (Failure RAEEvaluationFailure f, Field el)
              => RAE RadicalTuple OAFEReference el
              -> OAFEEvaluation el
              -> f (RAE RadicalSingleton el el)
evaluateRAE' (RAE muls adds c) oafeVals =
    do adds' <- mapM resolve adds
       muls' <- mapM resolveTuple muls
       return $! RAE muls' adds' c
    where resolveTuple:: (RadicalTuple OAFEReference,RadicalTuple OAFEReference)
                      -> f (RadicalSingleton el, RadicalSingleton el)
          resolveTuple (RT (oarefl1, oarefl2), RT (oarefr1, oarefr2)) =
              do l1 <- resolve oarefl1
                 l2 <- resolve oarefl2
                 r1 <- resolve oarefr1
                 r2 <- resolve oarefr2
                 let !l = l1 + l2
                     !r = r1 + r2
                 return $! (RS l, RS r)
          resolve :: OAFEReference
                  -> f el
          resolve (OAFERef var idx) =
              case HM.lookup var oafeVals of
                Nothing -> failure $ UnknownVariable var "evaluateRAE'"
                Just vals ->
                    if V.length vals <= idx
                       then failure $
                                IndexOutOfBounds var idx "evaluateRAE'"
                       else return $! vals V.! idx

-- | Evaluate one @RAE@.
--
-- In real-world use by the functionality David.
evalRAE :: Field el
         => RAE RadicalTuple OAFEReference el
         -> OAFEEvaluation el
         -> Either RAEEvaluationFailure el
evalRAE = evaluateRAE


-- | Evaluate one @RAE@.
--
-- In real-world use by the functionality David.
-- (same as @evalRAE@ but not exported and with @Failure@ type)
evaluateRAE :: forall f. forall el.
                (Failure RAEEvaluationFailure f, Field el)
             => RAE RadicalTuple OAFEReference el
             -> OAFEEvaluation el
             -> f el
evaluateRAE rae vals =
   do rae' <- evaluateRAE' rae vals
      let (RAE muls adds c) = rae'
          muls' :: [el]
          muls' = map multiplyRadicals muls
          multiplyRadicals :: (RadicalSingleton el, RadicalSingleton el) -> el
          multiplyRadicals (RS l, RS r) = l * r
          rMuls :: el
          rMuls = foldl' (+) zero muls'
          rAdds :: el
          rAdds = foldl' (+) zero adds
      return $! rMuls + rAdds + c

-- | Execute one @DRACFragment@.
--
-- Used internally only.
execDRACFragment :: (Field el)
                 => DRACFragment el -> (RunDRACStateMonad el) (Maybe (el, el))
execDRACFragment (outVar, drae) =
    do varMap <- get
       let !valsM = decodeDRAE varMap drae
           varMap''' =
              case valsM of
               Just (valL, valR) ->
                  let varMap' = M.insert (dvnLeftVarName outVar) valL varMap
                      varMap'' =
                          if outVar == _SPECIAL_DUAL_VAR_PRE_OUT_
                             then M.insert _SPECIAL_VAR_ADDED_PRE_OUT_
                                           (valL + valR)
                                           varMap'
                             else varMap'
                   in M.insert (dvnRightVarName outVar) valR varMap''
               Nothing -> varMap
       put varMap'''
       return valsM

-- | Execute one @RACFragment@.
--
-- Used internally only.
execRACFragment :: (Failure RAEEvaluationFailure f, Field el)
                => OAFEConfiguration el
                -> RACFragment el
                -> (RunRACStateMonad f el) el
execRACFragment oac (outVar, rae) =
    do varMap <- get
       val <- lift $ evaluateRAE rae varMap
       oae <- lift $ evaluateOAFE oac varMap (outVar, val)
       oae' <- lift $ possiblyCalculateAddedPreOutVar oae
       put oae'
       return $! val
    where
      possiblyCalculateAddedPreOutVar oae =
          let svl = _SPECIAL_VAR_PRE_OUT_LEFT_
              svr = _SPECIAL_VAR_PRE_OUT_RIGHT_
              svalM = if outVar == svl || outVar == svr
                         then case ( liftM V.toList $ HM.lookup svl oae
                                   , liftM V.toList $ HM.lookup svr oae
                                   ) of
                                (Just [valL], Just [valR]) ->
                                    return $! valL + valR
                                _ -> Nothing
                         else Nothing
           in case svalM of
                Just sval ->
                    evaluateOAFE oac oae (_SPECIAL_VAR_ADDED_PRE_OUT_, sval)
                Nothing -> return $! oae

oneZeroV :: Field el => Vector (el, el)
oneZeroV = V.singleton (one, zero)

-- | The special variable that contains the overall output (left part).
_SPECIAL_VAR_OUT_LEFT_ :: VariableName
_SPECIAL_VAR_OUT_LEFT_ = dvnLeftVarName _SPECIAL_DUAL_VAR_OUT_

-- | The special variable that contains the overall output (right part).
_SPECIAL_VAR_OUT_RIGHT_ :: VariableName
_SPECIAL_VAR_OUT_RIGHT_ = dvnRightVarName _SPECIAL_DUAL_VAR_OUT_

-- | The special variable that contains the added output of the last reguar
-- @RAE@.
_SPECIAL_VAR_ADDED_PRE_OUT_ :: VariableName
_SPECIAL_VAR_ADDED_PRE_OUT_ = "__added_last_rae"

-- | The special variable that contains the output of the last regular @RAE@,
-- left part.
_SPECIAL_VAR_PRE_OUT_LEFT_ :: VariableName
_SPECIAL_VAR_PRE_OUT_LEFT_ = dvnLeftVarName _SPECIAL_DUAL_VAR_PRE_OUT_

-- | The special variable that contains the output of the last regular @RAE@,
-- left part.
_SPECIAL_VAR_PRE_OUT_RIGHT_ :: VariableName
_SPECIAL_VAR_PRE_OUT_RIGHT_ = dvnRightVarName _SPECIAL_DUAL_VAR_PRE_OUT_

-- | The special dual variable that contains the overall output.
_SPECIAL_DUAL_VAR_OUT_ :: DualVarName
_SPECIAL_DUAL_VAR_OUT_ = genDualVarName "__out"

-- | The special dual variable that contains the output of the last
-- regular @DRAE@.
_SPECIAL_DUAL_VAR_PRE_OUT_ :: DualVarName
_SPECIAL_DUAL_VAR_PRE_OUT_ = genDualVarName "__last_rae"
