{-# LANGUAGE BangPatterns #-}
module Main where

-- # STDLIB
import Control.Concurrent (forkIO, myThreadId, killThread)
import Control.Concurrent.STM.TMVar ( TMVar, newEmptyTMVar
                                    , takeTMVar, tryPutTMVar
                                    )
import Control.Exception.Base (finally, catch, IOException)
import Control.Monad (liftM, when, void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.STM (atomically)
import System.Environment (getArgs)

-- # SITE PACKAGES
import Control.Concurrent.STM.TBMChan (TBMChan, newTBMChan, closeTBMChan)
import Data.ByteString (ByteString)
import Data.Conduit.TMChan (sourceTBMChan, sinkTBMChan)
import Data.Conduit ( Conduit, MonadResource
                    , ($$), ($=), (=$=), yield
                    , runResourceT
                    )
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as CN
import qualified Data.Map as M
import qualified Data.Text as T

-- # LOCAL
import Data.OAFE (OAFEEvaluationRequest, OAFEEvaluationResponse)
import Data.Helpers (takeOneConduit, runTCPClientNoWait)
import Data.LinearExpression (VarMapping)
import Data.RAE.Conduit ( oafeEvaluationResponseParseConduit
                        , oafeEvaluationRequestSerializeConduit
                        , racFragmentParseConduit
                        )
import Functionality.SetupPhase ( SetupGoliathToDavid, sd2gSerializeConduit
                                , sg2dParseConduit, setupD2GFromClientSettings
                                , clientSettingsFromSetupG2D
                                , sd2gSerializeConduit
                                )
import Functionality.David (runRACEvaluation)

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
-- 'reqs' and 'rsps' channels to the evaluation thread
-- (see @Functionality.David.runRACEvaluation@).
spawnCommThreads :: CN.ClientSettings RMonad
                 -> TBMChan (OAFEEvaluationRequest Element)
                 -> TBMChan (OAFEEvaluationResponse Element)
                 -> IO ()
spawnCommThreads tokenSettings reqs rsps =
    do _ <- forkIO $
              runResourceT (runTCPClientNoWait tokenSettings commApp)
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
-- | Start the evaluation server and the evaluation thread.
--
-- The evaluation server accepts an ARE stream from Goliath and transmits the
-- parsed AREs into the channel 'cRACFrag'
runEvaluator :: VarMapping Element
             -> TBMChan (OAFEEvaluationRequest Element)
             -> TBMChan (OAFEEvaluationResponse Element)
             -> TMVar (Maybe Element)
             -> DieCommand
             -> IO ()
runEvaluator varMap reqs rsps vResult die =
    do vStop <- atomically newEmptyTMVar
       cRACFrag <- atomically $ newTBMChan _INCOMING_ARE_CHAN_SIZE_
       _ <- forkIO $
             (runRACEvaluation varMap reqs rsps cRACFrag vResult putStrLn)
               `catch` (\e -> die $ show (e :: IOException))
       runResourceT
           (CN.runTCPServer _SRV_CONF_DAVID_FROM_GOLIATH_ (conn vStop cRACFrag))
         `catch` (\e -> die $ show (e :: IOException))
       return ()
    where conn vStop cRACFrag appData =
              do let src = CN.appSource appData
                 success <- liftIO $ atomically $ tryPutTMVar vStop ()
                 when success $ conn' cRACFrag src
          conn' cRACFrag src =
              src
              $= racFragmentParseConduit
              $$ sinkTBMChan cRACFrag

-- | This function exchanges the initial settings with Goliath.
--
-- It will send our TCP host/port which accepts the AREs to Goliath
-- and will receive the Token's configuration as well.
--
-- This function blocks until it received the settings from Goliath
-- or fails.
exchangeConfWithGoliath :: IO (Either String SetupGoliathToDavid)
exchangeConfWithGoliath =
    do vSettings <- atomically newEmptyTMVar
       res <- tryConnect _CLIENT_CONF_DAVID_TO_GOLIATH_ vSettings
              `catch` (\e -> return $ Left $ show (e :: IOException))
       case res of
         Left err -> return $ Left err
         Right () ->
             do settings <- atomically $ takeTMVar vSettings
                return $ Right settings
    where tryConnect :: CN.ClientSettings RMonad
                     -> TMVar SetupGoliathToDavid
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
              liftIO $ atomically $ void (tryPutTMVar var val)

type DieCommand = String -> IO ()

main :: IO ()
main =
    do putStrLn "DAVID START"
       args <- getArgs
       let inputArg =
             case args of
               [x] -> x
               _ -> error $ "Usage: David INPUT-ELEMENT    # found: " ++
                            show args
       let !inputElement = read inputArg
       let varMap = M.fromList [(T.pack "x", inputElement)]
       cRequests <- atomically $ newTBMChan _ARE_EVAL_CHAN_SIZE_
       cResponses <- atomically $ newTBMChan _ARE_EVAL_CHAN_SIZE_
       vResult <- atomically newEmptyTMVar

       mainTid <- myThreadId
       let die :: DieCommand
           die e =
               do putStrLn $ "FATAL ERROR: " ++ e
                  putStrLn   "Exiting..."
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
             do putStrLn "OK"
                spawnCommThreads tokenSettings cRequests cResponses
                out <- atomically $ takeTMVar vResult
                putStrLn $ "DAVID DONE, final result = " ++ show out
