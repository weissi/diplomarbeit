{-# LANGUAGE ScopedTypeVariables #-}
module Functionality.David (runRACEvaluation) where

-- # STDLIB
import Control.Concurrent.STM.TMVar (TMVar, putTMVar)
import Control.Monad (liftM, when)
import Control.Monad.STM (atomically)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)

-- # SITE PACKAGES
import Control.Concurrent.STM.TBMChan ( TBMChan
                                      , readTBMChan, writeTBMChan
                                      , tryReadTBMChan
                                      )
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

-- # LOCAL
import Data.FieldTypes (Field)
import Data.LinearExpression (VarMapping, VariableName)
import Data.OAFE ( OAFEEvaluationRequest, OAFEEvaluationResponse
                 , OAFEEvaluation
                 )
import Data.RAE.Evaluation ( evalRAE, _SPECIAL_VAR_OUT_, _SPECIAL_VAR_PRE_OUT_
                           , _SPECIAL_VAR_ADDED_PRE_OUT_
                           )
import Data.RAE.Types (RACFragment, leftVar, rightVar)

-- | This is the main functionality of David: Evaluating AREs.
--
-- It pulls the next ARE from the channel 'cRACFrag'. After having evaluated the
-- ARE, its output gets sent to the Token. The communication threads (see
-- 'spawnCommThreads') do the real work. This function communication with the
-- communication threads via the channels 'reqs' and 'rsps'. The OAFE evaluation
-- requests get pushed to 'reqs', after the Token has returned the evaluations,
-- they are received from 'rsps' and saved in a 'Map'.
runRACEvaluation :: forall el. Field el
                 => VarMapping el
                 -> TBMChan (OAFEEvaluationRequest el)
                 -> TBMChan (OAFEEvaluationResponse el)
                 -> TBMChan (RACFragment el)
                 -> TMVar (Maybe el)
                 -> (String -> IO ())
                 -> IO ()
runRACEvaluation varMap reqs rsps cRACFrag vResult logMsg =
    do oaeRef <- newIORef =<< evaluateInitialVars (M.toList varMap)
       loop oaeRef
    where loop oaeRef =
              do areStmt <- atomically $ readTBMChan cRACFrag
                 case areStmt of
                   Just (var, rae) ->
                      do logMsg ("Next ARE evaluates variable " ++
                                T.unpack var ++ ":")
                         oae <- if var == leftVar _SPECIAL_VAR_OUT_
                                   then readIORef oaeRef >>= doPreOutAddition
                                   else readIORef oaeRef
                         case evalRAE rae oae of
                           Left err -> fail $ "ARE eval failure: " ++ show err
                           Right val ->
                               do logMsg " --> ARE evaluation done"
                                  atomically $ writeTBMChan reqs (var, val)
                                  logMsg " --> OAFE eval request sent"
                                  oae' <- fetchResponse (Just var) oae
                                  logMsg " --> OAFE eval response received"
                                  writeIORef oaeRef oae'
                         loop oaeRef
                   Nothing ->
                      do oae <- readIORef oaeRef
                         case ( liftM V.toList $
                                HM.lookup (leftVar _SPECIAL_VAR_OUT_) oae
                              , liftM V.toList $
                                HM.lookup (rightVar _SPECIAL_VAR_OUT_) oae
                              ) of
                           (Just [l], Just [r]) ->
                               if l == r
                                  then atomically $ putTMVar vResult (Just l)
                                  else fail "The impossible happened!outL!=outR"
                           _ -> atomically $ putTMVar vResult Nothing
          doPreOutAddition :: OAFEEvaluation el
                           -> IO (OAFEEvaluation el)
          doPreOutAddition oae =
              let svl = leftVar _SPECIAL_VAR_PRE_OUT_
                  svr = rightVar _SPECIAL_VAR_PRE_OUT_
               in case ( liftM V.toList $ HM.lookup svl oae
                       , liftM V.toList $ HM.lookup svr oae
                       ) of
                    (Just [valL], Just [valR]) ->
                        do atomically $
                             writeTBMChan reqs
                                          ( _SPECIAL_VAR_ADDED_PRE_OUT_
                                          , valL+valR
                                          )
                           fetchResponse (Just _SPECIAL_VAR_ADDED_PRE_OUT_) oae
                    _ -> fail "Pre out variables not in OAE" >> undefined
          fetchResponse :: Maybe VariableName
                        -> OAFEEvaluation el
                        -> IO (OAFEEvaluation el)
          fetchResponse force oae =
              do res <- if isJust force
                           then liftM Just (atomically $    readTBMChan rsps)
                           else             atomically $ tryReadTBMChan rsps
                 case res of
                   Just (Just (var,val)) ->
                       let force' =
                             case force of
                               Just forceVar ->
                                   if forceVar==var || forceVar `HM.member` oae
                                      then Nothing
                                      else Just forceVar
                               Nothing -> Nothing
                        in fetchResponse force' (HM.insert var val oae)
                   Just Nothing ->
                       return oae
                   Nothing ->
                       fail "FUCK, channel closed"
          evaluateInitialVars initialVars =
              evaluateInitialVars' initialVars HM.empty
              where evaluateInitialVars' vars oae =
                        case vars of
                          [] -> return oae
                          ((var, val):vars') ->
                              do when (T.any (== '_') var)
                                      (fail "Underscores in vars disallowed!")
                                 atomically $ writeTBMChan reqs (var, val)
                                 oae' <- fetchResponse (Just var) oae
                                 evaluateInitialVars' vars' oae'

