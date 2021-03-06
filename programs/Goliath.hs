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
import Control.Concurrent.STM.TChan (TChan, newTChan, readTChan, writeTChan)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.STM (atomically)
import Control.Monad.Trans.Resource (MonadResource, runResourceT)
import Data.List (partition)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hFlush, stdout)
import qualified Control.Exception.Base as E
import qualified Data.ByteString as BS

-- # SITE PACKAGES
import Crypto.Random (SystemRandom, newGenIO)
import Data.Conduit ( Conduit
                    , ($$), ($=), (=$=)
                    )
import Data.Vector (Vector)
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as CN
import qualified Data.HashMap.Strict as HM

-- # LOCAL
import Data.ExpressionTypes (Expr(..))
import Data.RAE.Encoder (exprToRAC)
import Data.RAE.Types (RAC, VariableName)
import Data.OAFE (OAFEConfiguration)
import Data.RAE.Conduit ( oafeConfigSerializeConduit
                        , racFragmentSerializeConduit
                        )
import Data.Helpers (takeOneConduit, isOptionArg)
import Functionality.Goliath (readExprFromFile)
import Functionality.SetupPhase ( SetupDavidToGoliath(..)
                                , SetupGoliathToDavid(..)
                                , sg2dSerializeConduit, sd2gParseConduit
                                , setupG2DFromClientSettings
                                , clientSettingsFromSetupD2G
                                )
import qualified Math.Polynomials as P

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

debugPrintConduit :: (MonadResource m, Show a) => Conduit a m a
debugPrintConduit = CL.mapM $ \a -> liftIO (print a) >> return a

_DEBUG_ :: Bool
_DEBUG_ = False

configureToken :: CN.ClientSettings
               -> OAFEConfiguration Element
               -> IO ()
configureToken tokenSettings oac =
    do putStr "Sending OAC to Token... "
       CN.runTCPClient tokenSettings confTokenApp
       putStrLn "OK"
    where confTokenApp :: CN.AppData -> IO ()
          confTokenApp appData =
              let sink = CN.appSink appData
               in runResourceT $
                   CL.sourceList (HM.toList oac)
                   $= (if _DEBUG_ then debugPrintConduit else CL.map id)
                   =$= oacSerializeConduit
                   $$ sink

configureDavid :: SetupDavidToGoliath
               -> RAC Element
               -> IO ()
configureDavid sd2g rac =
   let davidNetConf = clientSettingsFromSetupD2G sd2g
   in (CN.runTCPClient davidNetConf connected)
        `E.catch` (\e -> putStrLn $ "Connect to David failed: " ++
                                    show (e :: E.IOException))
   where connected appData =
             let sink = CN.appSink appData
              in runResourceT $
                  CL.sourceList rac
                  $= (if _DEBUG_ then debugPrintConduit else CL.map id)
                  =$= racFragmentSerializeConduit
                  $$ sink

runGoliath :: CN.ClientSettings
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

evalClient :: TChan TokenConfInfo -> Expr Element -> CN.AppData -> IO ()
evalClient cTokens expr appData =
    let src = CN.appSource appData
        sink = CN.appSink appData
     in runResourceT $
         src
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

spawnTokenGenerator :: TChan TokenConfInfo -> IO ()
spawnTokenGenerator cTokens =
    do _ <- forkIO $
            do atomically $ writeTChan cTokens
                   ( _CLIENT_CONF_GOLIATH_TO_TOKEN_
                   , setupG2DFromClientSettings _CLIENT_CONF_DAVID_TO_TOKEN_
                   )
               return ()
       return ()

main :: IO ()
main =
    do putStrLn "GOLIATH START"
       rawArgs <- getArgs
       let (optionArgs, args) = partition isOptionArg rawArgs
           filePath = case args of
                        [x] -> x
                        _ -> error $ "Usage: Goliath [-h|-m] FILE   # found: "++
                                     show args
       buildPoly <- if "-m" `elem` optionArgs
                       then do putStrLn "POLY BUILDING: Monomial"
                               return P.monomial
                       else putStrLn "POLY BUILDING: Horner" >> return P.horner
       expr <- readExprFromFile (buildPoly _X_) filePath
       when _DEBUG_ (print expr)
       cTokens <- atomically newTChan
       spawnTokenGenerator cTokens
       putStrLn "GOLIATH READY FOR CONNECTION"
       hFlush stdout
       _ <- CN.runTCPServer _SRV_CONF_GOLIATH_FROM_DAVID_
                            (evalClient cTokens expr)
       putStrLn "GOLIATH DONE"
