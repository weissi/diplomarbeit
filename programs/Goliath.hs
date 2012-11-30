module Main where

-- # STDLIB
import Control.Concurrent (forkIO)
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Exception.Base (IOException, catch)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Data.List (foldl')
import Data.Text (Text)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import qualified Data.ByteString as BS
import qualified Data.Text as T

-- # SITE PACKAGES
import Crypto.Random (SystemRandom, newGenIO)
import Data.Conduit ( Conduit, MonadResource
                    , ($$), ($=), (=$=)
                    , runResourceT
                    )
import Data.Vector (Vector)
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as CN
import qualified Data.Conduit.Text as CT
import qualified Data.HashMap.Strict as HM

-- # LOCAL
import Data.ExpressionTypes (Expr(..))
import Data.RAE.Encoder (exprToRAC)
import Data.RAE.Types (RAC, VariableName)
import Data.OAFE (OAFEConfiguration)
import Data.RAE.Conduit ( oafeConfigSerializeConduit
                        , racFragmentSerializeConduit
                        )
import Data.Helpers (takeOneConduit)
import Functionality.SetupPhase ( SetupDavidToGoliath(..)
                                , SetupGoliathToDavid(..)
                                , sg2dSerializeConduit, sd2gParseConduit
                                , setupG2DFromClientSettings
                                , clientSettingsFromSetupD2G
                                )

import StaticConfiguration

-- | Token Configuration Port (G -> T) and Token Evaluation Port (D -> T)
type TokenConfInfo m = (CN.ClientSettings m, SetupGoliathToDavid)

die :: String -> IO ()
die err =
    do putStrLn $ "ERROR: " ++ err
       exitWith $ ExitFailure 1

oacSerializeConduit :: MonadResource m
                    => Conduit (VariableName, Vector (Element, Element))
                               m BS.ByteString
oacSerializeConduit = oafeConfigSerializeConduit

debugPrintConduit :: (MonadResource m, Show a) => Conduit a m a
debugPrintConduit = CL.mapM $ \a -> liftIO (print a) >> return a

_DEBUG_ :: Bool
_DEBUG_ = False

configureToken :: CN.ClientSettings RMonad
               -> OAFEConfiguration Element
               -> IO ()
configureToken tokenSettings oac =
    do putStr "Sending OAC to Token... "
       runResourceT$ CN.runTCPClient tokenSettings confTokenApp
       putStrLn "OK"
    where confTokenApp :: MonadResource m => CN.Application m
          confTokenApp appData =
              let sink = CN.appSink appData
               in CL.sourceList (HM.toList oac)
                  $= (if _DEBUG_ then debugPrintConduit else CL.map id)
                  =$= oacSerializeConduit
                  $$ sink

configureDavid :: SetupDavidToGoliath
               -> RAC Element
               -> IO ()
configureDavid sd2g rac =
   let davidNetConf = clientSettingsFromSetupD2G sd2g
   in runResourceT (CN.runTCPClient davidNetConf connected)
        `catch` (\e -> putStrLn $ "Connect to David failed: " ++
                                show (e :: IOException))
   where connected appData =
             let sink = CN.appSink appData
              in CL.sourceList rac
                 $= (if _DEBUG_ then debugPrintConduit else CL.map id)
                 =$= racFragmentSerializeConduit
                 $$ sink

runGoliath :: CN.ClientSettings RMonad
           -> SetupDavidToGoliath
           -> Expr Element
           -> IO ()
runGoliath tokenConf setupD2G expr =
    do g <- newGenIO :: IO SystemRandom
       putStrLn "Generating RAC fragments..."
       let (errM, rac, oac) = exprToRAC g expr
       putStrLn "Setting up Token..."
       _ <- forkIO $ configureToken tokenConf oac
       putStrLn "Setting up Token: OK"
       putStrLn "Setting up David..."
       configureDavid setupD2G rac
       putStrLn "Setting up David: OK"
       case errM of
         Left err -> putStrLn "ERROR" >> die ("ERROR: " ++ show err)
         Right _ -> putStrLn "OK"

evalClient :: MonadResource m
           => TChan (TokenConfInfo RMonad) -> Expr Element -> CN.Application m
evalClient cTokens expr appData =
    let src = CN.appSource appData
        sink = CN.appSink appData
     in src
        $= sd2gParseConduit
        =$= takeOneConduit
        =$= CL.mapM newEvalClient
        =$= sg2dSerializeConduit
        $$ sink
    where newEvalClient setupD2G =
              do liftIO $ putStrLn "David connected"
                 myToken <- liftIO $ atomically $ readTChan cTokens
                 _ <- liftIO $ forkIO $ runGoliath (fst myToken) setupD2G expr
                 return (snd myToken)

spawnTokenGenerator :: TChan (TokenConfInfo RMonad) -> IO ()
spawnTokenGenerator cTokens =
    do _ <- forkIO $
            do atomically $ writeTChan cTokens
                   ( _CLIENT_CONF_GOLIATH_TO_TOKEN_
                   , setupG2DFromClientSettings _CLIENT_CONF_DAVID_TO_TOKEN_
                   )
               return ()
       return ()

readExprFromFile :: FilePath -> IO (Expr Element)
readExprFromFile path =
    do elems <-
        runResourceT $
            CB.sourceFile path
            $= CT.decode CT.utf8
            =$= CT.lines
            =$= CL.mapM parseElements
            $$ CL.consume
       let powers = [0..] :: [Integer]
           powersOfX = map (\p -> _X_^p) powers
           exprTuples :: [(Expr Element, Expr Element)]
           exprTuples = zip (map Literal elems) powersOfX
           exprs :: [Expr Element]
           exprs = map (uncurry (*)) exprTuples
           expr :: Expr Element
           expr = foldl' (+) (fromInteger 0) exprs
       when _DEBUG_ (print expr)
       return expr
    where parseElements :: Monad m => Text -> m Element
          parseElements str =
              let parsed = readsPrec 0 (T.unpack str)
               in case parsed of
                    ((v, _):_) -> return $! v
                    _ -> fail "Parse failed"

main :: IO ()
main =
    do putStrLn "GOLIATH START"
       args <- getArgs
       let filePath = case args of
                        [x] -> x
                        _ -> error $ "Usage: Goliath FILE    # found: " ++
                                     show args
       expr <- readExprFromFile filePath
       cTokens <- atomically newTChan
       spawnTokenGenerator cTokens
       runResourceT $ CN.runTCPServer _SRV_CONF_GOLIATH_FROM_DAVID_
                                      (evalClient cTokens expr)
       putStrLn "GOLIATH DONE"
