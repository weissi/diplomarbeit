{-# LANGUAGE ScopedTypeVariables #-}
-- | The all in one functionality. This functionality evaluates an @Expr@ just
-- as the usual functionalities would but does eveything in one binary.
--
-- This functionality is not meant for productition. It does not have the
-- networking nor the serialization/deserialization overhead.
module Functionality.AllInOne (evaluateExpr) where

-- # Standard Library
import Control.Concurrent (forkIO, killThread)
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TMVar (newTMVar, newEmptyTMVar, takeTMVar)

-- # Site Packages
import Control.Concurrent.STM.TBMChan (TBMChan, newTBMChan)
import Control.Monad.CryptoRandom (CRandom(..))
import Control.Monad.IO.Class (liftIO)
import Crypto.Random (SystemRandom, newGenIO)
import Data.Conduit (($=), ($$))
import Data.Conduit.List (sourceList)
import Data.Conduit.TMChan (sourceTBMChan, sinkTBMChan)
import qualified Data.Conduit.List as CL

-- # Local
import Data.ExpressionTypes (Expr(..))
import Data.FieldTypes (Field)
import Data.RAE.Types (RAC, RACFragment, VarMapping)
import Data.RAE.Encoder (exprToRAC)
import Functionality.David (runRACEvaluation)
import Functionality.Token (runOAFEEvaluation)

_CHAN_SIZE_ :: Int
_CHAN_SIZE_ = 64

evaluateExpr :: forall el. (Show el, CRandom el, Field el)
             => VarMapping el -> Expr el -> (String -> IO ()) -> IO (Maybe el)
evaluateExpr varMap expr logMsg =
    do -- start goliath
       (rac, oac) <- goliath
       -- setup OAFE configuration var
       vOAC <- atomically $ newTMVar oac
       -- setup channels
       david2token <- atomically $ newTBMChan _CHAN_SIZE_
       token2david <- atomically $ newTBMChan _CHAN_SIZE_
       goliath2david <- atomically $ newTBMChan _CHAN_SIZE_
       -- setup result var
       vResult <- atomically newEmptyTMVar
       -- start token in new thread
       tokenTid <- forkIO (token vOAC david2token token2david)
       -- start pusher in new thread
       pushTid <- forkIO (pushRAC rac goliath2david)
       -- run david
       logMsg "RUNNING DAVID"
       runRACEvaluation varMap
                        david2token
                        token2david
                        goliath2david
                        vResult
                        logMsg
       logMsg "DAVID DONE"
       -- kill threads
       killThread tokenTid
       killThread pushTid
       -- fetch result
       result <- atomically $ takeTMVar vResult
       return $! result
    where goliath =
              do g <- newGenIO :: IO SystemRandom
                 let (errM, rac, oac) = exprToRAC g expr
                 case errM of
                   Left err -> fail $ show err
                   Right _ -> return (rac, oac)
          token vOAC d2t t2d =
              do sourceTBMChan d2t
                 $= CL.mapM (liftIO . (runOAFEEvaluation vOAC))
                 $$ sinkTBMChan t2d
          pushRAC :: RAC el -> TBMChan (RACFragment el) -> IO ()
          pushRAC rac g2d = sourceList rac $$ sinkTBMChan g2d
