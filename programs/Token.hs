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
module Main where

-- # STDLIB
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TMVar ( TMVar, isEmptyTMVar
                                    , putTMVar, newEmptyTMVar
                                    )
import Control.Monad.STM (atomically)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- # SITE PACKAGES
import Control.Monad.Trans.Resource (ResourceT)
import Data.Conduit ( Conduit, MonadResource, runResourceT
                    , ($=), ($$), yield
                    )
import Data.Conduit.Network (Application)
import Data.Vector (Vector)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as CN
import qualified Data.HashMap.Strict as HM
import qualified Network.Socket as NS

-- # LOCAL
import Data.OAFE (OAFEConfiguration, OAFEEvaluationResponse)
import Data.RAE.Conduit ( oafeConfigParseConduit
                        , oafeEvaluationRequestParseConduit
                        , oafeEvaluationResponseSerializeConduit
                        )
import Data.LinearExpression (VariableName)
import Functionality.Token (runOAFEEvaluation)

import StaticConfiguration

configParseConduit :: MonadResource m
                   => Conduit ByteString m
                              (VariableName, Vector (Element, Element))
configParseConduit = oafeConfigParseConduit

evalRequestParseConduit :: MonadResource m
                        => Conduit ByteString m (VariableName, Element)
evalRequestParseConduit = oafeEvaluationRequestParseConduit

evalResponseSerializeConduit :: MonadResource m
                             => Conduit (OAFEEvaluationResponse Element)
                                        m ByteString
evalResponseSerializeConduit = oafeEvaluationResponseSerializeConduit

tokenEvaluatorStartThread :: TMVar (OAFEConfiguration Element) -> IO ()
tokenEvaluatorStartThread vOAC =
    do putStrLn "TOKEN EVALUATOR RUNNING"
       let cfg :: CN.ServerSettings (ResourceT IO)
           cfg = _SRV_CONF_TOKEN_FROM_DAVID_ {
                   CN.serverAfterBind =
                       \s -> liftIO $ NS.setSocketOption s NS.NoDelay 1
                 }
       runResourceT (CN.runTCPServer cfg (tokenEval vOAC))

tokenEval :: (MonadResource m, MonadIO m)
          => TMVar (OAFEConfiguration Element) -> Application m
tokenEval vOAC appData =
    let src = CN.appSource appData
        sink = CN.appSink appData
     in src
        $= evalRequestParseConduit
        $= CL.mapM (liftIO . (runOAFEEvaluation vOAC))
        $= evalResponseSerializeConduit
        $$ sink

tokenClient :: forall m. (MonadResource m, MonadIO m)
            => TMVar () -> TMVar (OAFEConfiguration Element) -> Application m
tokenClient vAcceptConfig vOAC appData =
    do let source = CN.appSource appData
           sink = CN.appSink appData
       accept <- liftIO $ atomically $
           do empty <- isEmptyTMVar vAcceptConfig
              if empty
                then putTMVar vAcceptConfig () >> return True
                else return False
       if accept
         then do m <- receiver source
                 sender sink m
         else CL.sourceList [BS.pack [70, 85, 76, 76, 10]] $$ sink
    where launchTokenEvaluation oac =
              do atomically $ putTMVar vOAC oac
                 return ()
          receiver src =
              do l <- src $= configParseConduit $$ CL.consume
                 return $ HM.fromList l
          sender sink m =
              do liftIO $ launchTokenEvaluation m
                 yield (BS.pack [79, 75, 10]) $$ sink


main :: IO ()
main =
    do putStrLn "TOKEN: START"
       vAcceptConfig <- atomically newEmptyTMVar
       vOAC <- atomically newEmptyTMVar
       _ <- forkIO $ tokenEvaluatorStartThread vOAC
       runResourceT $ CN.runTCPServer _SRV_CONF_TOKEN_FROM_GOLIATH_
                                      (tokenClient vAcceptConfig vOAC)
       putStrLn "TOKEN: DONE"
