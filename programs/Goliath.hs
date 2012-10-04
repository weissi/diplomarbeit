module Main where

-- # STDLIB
import Prelude hiding (catch)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Exception.Base (IOException, catch)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import System.Exit (exitWith, ExitCode(..))
import qualified Data.ByteString as BS

-- # SITE PACKAGES
import Crypto.Random (SystemRandom, newGenIO)
import Data.Conduit ( Conduit, MonadResource
                    , ($$), ($=), (=$=)
                    , runResourceT
                    )
import Data.Vector (Vector)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as CN
import qualified Data.HashMap.Strict as HM

-- # LOCAL
import Codec.DARE (exprToRP)
import Data.DAREEvaluation ( OAFEConfiguration, ERP(..), EDARE(..)
                           , VariableName
                           , prepareRPEvaluation
                           , OAFEReference
                           )
import Data.OAFEComm (oafeConfigSerializeConduit, areSerializeConduit)
import Data.Helpers (takeOneConduit)
import Data.SetupPhase ( SetupDavidToGoliath(..), SetupGoliathToDavid(..)
                       , sg2dSerializeConduit, sd2gParseConduit
                       , setupG2DFromClientSettings
                       , clientSettingsFromSetupD2G
                       )

import StaticConfiguration

-- | Token Configuration Port (G -> T) and Token Evaluation Port (D -> T)
type TokenConfInfo = (CN.ClientSettings, SetupGoliathToDavid)

die :: String -> IO ()
die err =
    do putStrLn $ "ERROR: " ++ err
       exitWith $ ExitFailure 1

oacSerializeConduit :: MonadResource m
                    => Conduit (VariableName, Vector (Element, Element))
                               m BS.ByteString
oacSerializeConduit = oafeConfigSerializeConduit

configureToken :: CN.ClientSettings -> OAFEConfiguration Element -> IO ()
configureToken tokenSettings oac =
    do putStr "Sending OAC to Token... "
       runResourceT$ CN.runTCPClient tokenSettings confTokenApp
       putStrLn "OK"
    where confTokenApp :: MonadResource m => CN.Application m
          confTokenApp _ sink =
              CL.sourceList (HM.toList oac)
              $= oacSerializeConduit
              $$ sink

configureDavid :: SetupDavidToGoliath
               -> [(VariableName, EDARE OAFEReference Element)]
               -> IO ()
configureDavid sd2g edares =
   let davidNetConf = clientSettingsFromSetupD2G sd2g
   in (runResourceT $ CN.runTCPClient davidNetConf connected)
      `catch` (\e -> putStrLn $ "Connect to David failed: " ++
                                show (e :: IOException))
   where connected _ sink =
             CL.sourceList edares
             $= areSerializeConduit
             $$ sink

runGoliath :: CN.ClientSettings -> SetupDavidToGoliath -> IO ()
runGoliath tokenConf setupD2G =
    do g <- (newGenIO :: IO SystemRandom)
       putStrLn "Generating DARES..."
       let (errM, dares) = exprToRP g _TEST_EXPR_
       let erp = prepareRPEvaluation dares
           oac = erpOAFEConfig erp
           edares = erpEDares erp
       putStrLn "Setting up Token..."
       configureToken tokenConf oac
       putStrLn "Setting up Token: OK"
       putStrLn "Setting up David..."
       configureDavid setupD2G edares
       putStrLn "Setting up David: OK"
       case errM of
         Left err -> putStrLn "ERROR" >> die ("ERROR: " ++ show err)
         Right _ -> putStrLn "OK"

evalClient :: MonadResource m => TChan TokenConfInfo -> CN.Application m
evalClient cTokens src sink =
    src
    $= sd2gParseConduit
    =$= takeOneConduit
    =$= CL.mapM newEvalClient
    =$= sg2dSerializeConduit
    $$ sink
    where newEvalClient setupD2G =
              do liftIO $ putStrLn "David connected"
                 myToken <- liftIO $ atomically $ readTChan cTokens
                 _ <- liftIO $ forkIO $ runGoliath (fst myToken) setupD2G
                 return (snd myToken)

spawnTokenGenerator :: TChan TokenConfInfo -> IO ()
spawnTokenGenerator cTokens =
    do _ <- forkIO $
            do atomically $ writeTChan cTokens $
                   ( _CLIENT_CONF_GOLIATH_TO_TOKEN_
                   , setupG2DFromClientSettings _CLIENT_CONF_DAVID_TO_TOKEN_
                   )
               return ()
       return ()

main :: IO ()
main =
    do putStrLn "GOLIATH START"
       cTokens <- atomically $ newTChan
       spawnTokenGenerator cTokens
       runResourceT $ CN.runTCPServer _SRV_CONF_GOLIATH_FROM_DAVID_
                                      (evalClient cTokens)
       putStrLn "GOLIATH DONE"
