module Main where

-- # STDLIB
import Control.Concurrent (forkIO, myThreadId, killThread)
import Control.Concurrent.STM.TMVar ( TMVar, newEmptyTMVar, putTMVar
                                    , takeTMVar, tryPutTMVar
                                    )
import Control.Exception.Base (finally, catch, IOException)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.STM (atomically)
import Control.Monad (liftM)
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)

-- # SITE PACKAGES
import Control.Concurrent.STM.TBMChan ( TBMChan
                                      , newTBMChan, readTBMChan, writeTBMChan
                                      , closeTBMChan, tryReadTBMChan
                                      )
import Data.ByteString (ByteString)
import Data.Conduit.TMChan (sourceTBMChan, sinkTBMChan)
import Data.Conduit ( Conduit, MonadResource
                    , ($$), ($=), (=$=), yield
                    , runResourceT
                    )
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as CN
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

-- # LOCAL
import Data.DAREEvaluation ( OAFEEvaluationRequest, OAFEEvaluationResponse
                           , EDARE(..), OAFEReference, evalDARE
                           , OAFEEvaluation
                           )
import Data.DARETypes ( _SPECIAL_VAR_OUT_, _SPECIAL_VAR_PRE_OUT_
                      , _SPECIAL_VAR_ADDED_PRE_OUT_
                      , leftVar, rightVar
                      )
import Data.Helpers (takeOneConduit)
import Data.LinearExpression (VarMapping, VariableName)
import Data.OAFEComm ( oafeEvaluationResponseParseConduit
                     , oafeEvaluationRequestSerializeConduit
                     , areParseConduit
                     )
import Data.SetupPhase ( SetupGoliathToDavid, sd2gSerializeConduit
                       , sg2dParseConduit, setupD2GFromClientSettings
                       , clientSettingsFromSetupG2D
                       , sd2gSerializeConduit
                       )

import StaticConfiguration

_ARE_EVAL_CHAN_SIZE_ :: Int
_ARE_EVAL_CHAN_SIZE_ = 16

_INCOMING_ARE_CHAN_SIZE_ :: Int
_INCOMING_ARE_CHAN_SIZE_ = 1000

evalRspParseConduit :: MonadResource m
                    => Conduit ByteString m (OAFEEvaluationResponse Element)
evalRspParseConduit = oafeEvaluationResponseParseConduit

evalReqSerializeConduit :: MonadResource m
                        => Conduit (OAFEEvaluationRequest Element) m ByteString
evalReqSerializeConduit = oafeEvaluationRequestSerializeConduit

-- | Spawn the communication threads between the Token and David (me).
--
-- The communication threads tasks are twofold:
-- One serializes and sends OAFE evaluation requests to the Token, the other
-- receives and parses OAFE evaluation responses. Both are connected via the
-- 'reqs' and 'rsps' channels to the evaluation thread (see 'evaluate').
spawnCommThreads :: CN.ClientSettings RMonad
                 -> TBMChan (OAFEEvaluationRequest Element)
                 -> TBMChan (OAFEEvaluationResponse Element)
                 -> IO ()
spawnCommThreads tokenSettings reqs rsps =
    do _ <- forkIO $
              (runResourceT $ CN.runTCPClient tokenSettings commApp)
              `finally` do atomically $ closeTBMChan reqs
                           atomically $ closeTBMChan rsps
       return ()
    where commApp appData =
              do let netSrc = CN.appSource appData
                     netSink = CN.appSink appData
                 _ <- liftIO $ forkIO $ runResourceT $ sender netSink
                 receiver netSrc
                 return ()
          sender netSink =
              sourceTBMChan reqs
              $= evalReqSerializeConduit
              $$ netSink
          receiver netSrc =
              netSrc
              $= evalRspParseConduit
              $$ sinkTBMChan rsps

-- | This is the main functionality of David: Evaluating AREs.
--
-- It pulls the next ARE from the channel 'cARE'. After having evaluated the
-- ARE, its output gets sent to the Token. The communication threads (see
-- 'spawnCommThreads') do the real work. This function communication with the
-- communication threads via the channels 'reqs' and 'rsps'. The OAFE evaluation
-- requests get pushed to 'reqs', after the Token has returned the evaluations,
-- they are received from 'rsps' and saved in a 'Map'.
evaluate :: VarMapping Element
         -> TBMChan (OAFEEvaluationRequest Element)
         -> TBMChan (OAFEEvaluationResponse Element)
         -> TBMChan (VariableName, EDARE OAFEReference Element)
         -> TMVar (Maybe Element)
         -> DieCommand
         -> IO ()
evaluate varMap reqs rsps cARE vResult die =
    do oaeRef <- newIORef =<< evaluateInitialVars (M.toList varMap)
       loop oaeRef
    where loop oaeRef =
              do areStmt <- atomically $ readTBMChan cARE
                 case areStmt of
                   Just (var, are) ->
                      do putStrLn ("Next ARE evaluates variable " ++
                                   T.unpack var ++ ":")
                         oae <- if var == leftVar _SPECIAL_VAR_OUT_
                                   then readIORef oaeRef >>= doPreOutAddition
                                   else readIORef oaeRef
                         case evalDARE are oae of
                           Left err -> die $ "ARE eval failure: " ++ show err
                           Right val ->
                               do putStrLn $ " --> ARE evaluation done"
                                  atomically $ writeTBMChan reqs (var, val)
                                  putStrLn $ " --> OAFE eval request sent"
                                  oae' <- (fetchResponse (Just var) oae)
                                  putStrLn $ " --> OAFE eval response received"
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
                                  else die "The impossible happened! outL!=outR"
                           _ -> atomically $ putTMVar vResult Nothing
          doPreOutAddition :: OAFEEvaluation Element
                           -> IO (OAFEEvaluation Element)
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
                    _ -> die "Pre out variables not in OAE" >> undefined
          fetchResponse :: Maybe VariableName
                        -> OAFEEvaluation Element
                        -> IO (OAFEEvaluation Element)
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
                              do atomically $ writeTBMChan reqs (var, val)
                                 oae' <- fetchResponse (Just var) oae
                                 evaluateInitialVars' vars' oae'

-- | Start the evaluation server and the evaluation thread.
--
-- The evaluation server accepts an ARE stream from Goliath and transmits the
-- parsed AREs into the channel 'cARE'
runEvaluator :: VarMapping Element
             -> TBMChan (OAFEEvaluationRequest Element)
             -> TBMChan (OAFEEvaluationResponse Element)
             -> TMVar (Maybe Element)
             -> DieCommand
             -> IO ()
runEvaluator varMap reqs rsps vResult die =
    do vStop <- atomically $ newEmptyTMVar
       cARE <- atomically $ newTBMChan _INCOMING_ARE_CHAN_SIZE_
       _ <- forkIO $ evaluate varMap reqs rsps cARE vResult die
       (runResourceT $
           CN.runTCPServer _SRV_CONF_DAVID_FROM_GOLIATH_ (conn vStop cARE))
         `catch` (\e -> die $ show (e :: IOException))
       return ()
    where conn vStop cARE appData =
              do let src = CN.appSource appData
                 success <- liftIO $ atomically $ tryPutTMVar vStop ()
                 if success
                    then conn' cARE src
                    else return ()
          conn' cARE src =
              src
              $= areParseConduit
              $$ sinkTBMChan cARE

-- | This function exchanges the initial settings with Goliath.
--
-- It will send our TCP host/port which accepts the AREs to Goliath
-- and will receive the Token's configuration as well.
--
-- This function blocks until it received the settings from Goliath
-- or fails.
exchangeConfWithGoliath :: IO (Either String SetupGoliathToDavid)
exchangeConfWithGoliath =
    do vSettings <- atomically $ newEmptyTMVar
       res <- (tryConnect _CLIENT_CONF_DAVID_TO_GOLIATH_ vSettings
                `catch` (\e -> return $ Left $ show (e :: IOException)))
       case res of
         Left err -> return $ Left err
         Right () ->
             do settings <- atomically $ takeTMVar vSettings
                return $ Right settings
    where tryConnect :: CN.ClientSettings RMonad
                     -> TMVar (SetupGoliathToDavid)
                     -> IO (Either String ())
          tryConnect conf vSettings =
              do runResourceT $ CN.runTCPClient conf (connected vSettings)
                 return $ Right ()
          connected vSettings appData =
              do let src = CN.appSource appData
                     sink = CN.appSink appData
                 () <- sender sink
                 receiver vSettings src
          sender sink =
              yield (setupD2GFromClientSettings _CLIENT_CONF_GOLIATH_TO_DAVID_)
              $= sd2gSerializeConduit
              $$ sink
          receiver vSettings src =
              src
              $= sg2dParseConduit
              =$= takeOneConduit
              $$ CL.mapM_ (putVar vSettings)
          putVar var val =
              liftIO $ (atomically $ tryPutTMVar var val >> return ())

type DieCommand = String -> IO ()

main :: IO ()
main =
    do putStrLn "DAVID START"
       let varMap = M.fromList [(T.pack "x", 1)]
       cRequests <- atomically $ newTBMChan _ARE_EVAL_CHAN_SIZE_
       cResponses <- atomically $ newTBMChan _ARE_EVAL_CHAN_SIZE_
       vResult <- atomically $ newEmptyTMVar

       mainTid <- myThreadId
       let die :: DieCommand
           die e =
               do putStrLn $ "FATAL ERROR: " ++ e
                  putStrLn $ "Exiting..."
                  atomically $ closeTBMChan cRequests
                  atomically $ closeTBMChan cResponses
                  killThread mainTid
                  error "Thread should be dead"
       _ <- forkIO $ runEvaluator varMap cRequests cResponses vResult die

       putStrLn "Trying to acquire token settings... "
       sg2dM <- exchangeConfWithGoliath
       case liftM clientSettingsFromSetupG2D sg2dM of
         Left err ->
             putStrLn $ "Failed: Could not connect to Goliath: " ++ err
         Right tokenSettings ->
             do putStrLn $ "OK"
                spawnCommThreads tokenSettings cRequests cResponses
                out <- atomically $ takeTMVar vResult
                putStrLn $ "DAVID DONE, final result = " ++ show out
