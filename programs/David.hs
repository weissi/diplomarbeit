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

{-# LANGUAGE BangPatterns #-}
module Main where

-- # STDLIB
import Control.Concurrent (forkIO, myThreadId, killThread)
import Control.Concurrent.STM.TMVar ( TMVar, newEmptyTMVar
                                    , takeTMVar, tryPutTMVar
                                    )
import Control.Monad (liftM, when, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Data.List (partition)
import System.Environment (getArgs)
import qualified Control.Exception.Base as E

-- # SITE PACKAGES
import Control.Concurrent.STM.TBMChan (TBMChan, newTBMChan, closeTBMChan)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Data.ByteString (ByteString)
import Data.Conduit.TMChan (sourceTBMChan, sinkTBMChan)
import Data.Conduit ( Conduit
                    , ($$), ($=), (=$=), yield
                    , Source
                    )
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as CN
import qualified Data.Map as M
import qualified Data.Text as T

-- # LOCAL
import Data.OAFE (OAFEEvaluationRequest, OAFEEvaluationResponse)
import Data.Helpers (takeOneConduit, isOptionArg, runTCPClientNoWait)
import Data.LinearExpression (VarMapping)
import Data.RAE.Conduit ( oafeEvaluationResponseParseConduit
                        , oafeEvaluationRequestSerializeConduit
                        , racFragmentParseConduit
                        )
import Data.RAE.Types (RACFragment)
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
spawnCommThreads :: CN.ClientSettings
                 -> TBMChan (OAFEEvaluationRequest Element)
                 -> TBMChan (OAFEEvaluationResponse Element)
                 -> IO ()
spawnCommThreads tokenSettings reqs rsps =
    do _ <- forkIO $
              (runTCPClientNoWait tokenSettings commApp)
              `E.finally` do atomically $ closeTBMChan reqs
                             atomically $ closeTBMChan rsps
       return ()
    where commApp :: CN.AppData -> IO ()
          commApp appData =
              do let netSource :: MonadResource m => Source m ByteString
                     netSource = CN.appSource appData
                     netSink = CN.appSink appData
                 _ <- liftIO $ forkIO $ runResourceT $ sender netSink
                 runResourceT $ receiver netSource
          sender netSink =
              sourceTBMChan reqs
              $= evalReqSerializeConduit
              $$ netSink
          receiver netSrc =
              netSrc
              $= evalRspParseConduit
              $$ sinkTBMChan rsps True
-- | Start the evaluation server and the evaluation thread.
--
-- The evaluation server accepts an ARE stream from Goliath and transmits the
-- parsed AREs into the channel 'cRACFrag'
runEvaluator :: VarMapping Element
             -> TBMChan (OAFEEvaluationRequest Element)
             -> TBMChan (OAFEEvaluationResponse Element)
             -> TMVar (Maybe Element)
             -> DieCommand
             -> (String -> IO())
             -> IO ()
runEvaluator varMap reqs rsps vResult logMsg die =
    do vStop <- atomically newEmptyTMVar
       cRACFrag <- atomically $ newTBMChan _INCOMING_ARE_CHAN_SIZE_
       _ <- forkIO $
             (runRACEvaluation varMap reqs rsps cRACFrag vResult logMsg)
               `E.catch` (\e -> die $ show (e :: E.IOException))
       (CN.runTCPServer _SRV_CONF_DAVID_FROM_GOLIATH_ (conn vStop cRACFrag))
         `E.catch` (\e -> die $ show (e :: E.IOException))
       return ()
    where conn :: TMVar () -> TBMChan (RACFragment Element) -> CN.AppData -> IO ()
          conn vStop cRACFrag appData =
              do let src = CN.appSource appData
                 success <- liftIO $ atomically $ tryPutTMVar vStop ()
                 when success $ runResourceT $ conn' cRACFrag src
          conn' cRACFrag src =
              src
              $= racFragmentParseConduit
              $$ sinkTBMChan cRACFrag True

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
              `E.catch` (\e -> return $ Left $ show (e :: E.IOException))
       case res of
         Left err -> return $ Left err
         Right () ->
             do settings <- atomically $ takeTMVar vSettings
                return $ Right settings
    where tryConnect :: CN.ClientSettings
                     -> TMVar SetupGoliathToDavid
                     -> IO (Either String ())
          tryConnect conf vSettings =
              do CN.runTCPClient conf (connected vSettings)
                 return $ Right ()
          connected vSettings appData =
              do let src = CN.appSource appData
                     sink = CN.appSink appData
                 () <- runResourceT $ sender sink
                 runResourceT $ receiver vSettings src
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
       rawArgs <- getArgs
       let (optionArgs, args) = partition isOptionArg rawArgs
           inputArg =
             case args of
               [x] -> x
               _ -> error $ "Usage: David [-q] INPUT-ELEMENT    # found: " ++
                            show args
           logMsg = if "-q" `elem` optionArgs
                       then \_ -> return ()
                       else putStrLn
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
       _ <- forkIO $ runEvaluator varMap cRequests cResponses vResult logMsg die

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
