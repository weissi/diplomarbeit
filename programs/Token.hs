module Main where

-- # STDLIB
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TMVar ( TMVar, isEmptyTMVar
                                    , putTMVar, takeTMVar
                                    , newEmptyTMVar
                                    )
import Control.Monad.STM (atomically)
import Control.Monad.IO.Class (MonadIO(..))
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

-- # SITE PACKAGES
import Data.Conduit ( Conduit, MonadResource, runResourceT
                    , ($=), (=$=), ($$)
                    )
import Data.Conduit.Network (Application)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as CN
import qualified Data.Map as M

-- # LOCAL
import Data.DAREEvaluation ( OAFEConfiguration, OAFEEvaluationRequest
                           , OAFEEvaluationResponse
                           , processOAFEEvaluationRequest
                           )
import Data.OAFEComm ( oafeConfigParseConduit, oafeConfigSerializeConduit
                     , oafeEvaluationRequestParseConduit
                     , oafeEvaluationResponseSerializeConduit)
import Data.Helpers (takeOneConduit)
import Data.LinearExpression (VariableName)

import StaticConfiguration

configParseConduit :: MonadResource m
                   => Conduit ByteString m (OAFEConfiguration Element)
configParseConduit = oafeConfigParseConduit

evalRequestParseConduit :: MonadResource m
                        => Conduit ByteString m (VariableName, Element)
evalRequestParseConduit = oafeEvaluationRequestParseConduit

evalResponseSerializeConduit :: MonadResource m
                             => Conduit (VariableName, [Element]) m ByteString
evalResponseSerializeConduit = oafeEvaluationResponseSerializeConduit

serializeConduit :: MonadResource m
                 => Conduit (OAFEConfiguration Element) m ByteString
serializeConduit = oafeConfigSerializeConduit

tokenEvaluatorStartThread :: TMVar (OAFEConfiguration Element) -> IO ()
tokenEvaluatorStartThread vOAC =
    do putStrLn "TOKEN EVALUATOR RUNNING"
       runResourceT ( CN.runTCPServer _SRV_CONF_TOKEN_FROM_DAVID_
                                      (tokenEval vOAC) )

tokenEval :: (MonadResource m, MonadIO m)
          => TMVar (OAFEConfiguration Element) -> Application m
tokenEval vOAC src sink =
    src
    $= evalRequestParseConduit
    =$= CL.mapM doOAFEEvaluation
    =$= evalResponseSerializeConduit
    $$ sink
    where
        doOAFEEvaluation :: MonadResource m
                         => OAFEEvaluationRequest Element
                         -> m (OAFEEvaluationResponse Element)
        doOAFEEvaluation req =
           do oac <- liftIO $ atomically $ takeTMVar vOAC
              let rsp = processOAFEEvaluationRequest oac req
                  var = fst rsp
                  oac' = M.delete var oac
              liftIO $ atomically $ putTMVar vOAC oac'
              return rsp

tokenClient :: (MonadResource m, MonadIO m)
            => TMVar () -> TMVar (OAFEConfiguration Element) -> Application m
tokenClient vAcceptConfig vOAC src sink =
    do accept <- liftIO $ atomically $
        do empty <- isEmptyTMVar vAcceptConfig
           if empty
              then putTMVar vAcceptConfig () >> return True
              else return False
       if accept
          then src
               $= configParseConduit
               =$= takeOneConduit
               =$= CL.mapM_ launchTokenEvaluation
               =$= CL.map (const (BS.pack [79, 75, 10]))
               $$ sink
          else CL.sourceList [BS.pack [70, 85, 76, 76, 10]] $$ sink
    where launchTokenEvaluation oac =
              liftIO $
              do atomically $ putTMVar vOAC oac
                 _ <- forkIO $ tokenEvaluatorStartThread vOAC
                 return ()

main :: IO ()
main =
    do putStrLn "TOKEN: START"
       vAcceptConfig <- atomically $ newEmptyTMVar
       vOAC <- atomically $ newEmptyTMVar
       runResourceT $ CN.runTCPServer _SRV_CONF_TOKEN_FROM_GOLIATH_
                                      (tokenClient vAcceptConfig vOAC)
       putStrLn "TOKEN: DONE"
