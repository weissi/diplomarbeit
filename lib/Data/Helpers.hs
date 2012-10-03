{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Helpers ( integralBytes, takeOneConduit
                    , runTCPServerNoWait, runTCPClientNoWait
                    ) where
import Blaze.ByteString.Builder (Builder, toByteString)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Control.Exception (finally, bracket)
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Data.Bits (Bits, shiftR)
import Data.ByteString
import Data.Char (chr)
import Data.Conduit.Network ( ClientSettings(..), Application
                            , ServerSettings(..)
                            , sourceSocket, sinkSocket
                            )
import Data.Monoid (mempty, mappend)
import qualified Data.Conduit as C
import qualified Data.Conduit.Util as CU
import qualified Data.Conduit.Network as CN
import qualified Network.Socket as NS

integralBytes :: (Integral a, Bits a, Show a) => a -> ByteString
integralBytes n0
  | n0 <  0   = error ("integralBytes: applied to negative number " ++ show n0)
  | otherwise = toByteString $ marshallIntByte n0 mempty
  where marshallIntByte :: (Show a, Bits a, Integral a)
                        => a -> Builder -> Builder
        marshallIntByte n acc =
           let !newChar = chr . fromIntegral $ n `mod` 256
               newBuilder = fromChar newChar
            in case n of
                 0 -> acc
                 _ -> marshallIntByte (n `shiftR` 8) (acc `mappend` newBuilder)

takeOneConduit :: C.MonadResource m => C.Conduit i m i
takeOneConduit =
    CU.conduitState () push close
    where push _ i = return $ CU.StateFinished Nothing [i]
          close _ = return []

-- | Run an @Application@ with the given settings. This function will create a
-- new listening socket, accept connections on it, and spawn a new thread for
-- each connection. Does TCP_NOWAIT.
--
-- (stolen from network-conduit-0.5.2)
runTCPServerNoWait :: (MonadIO m, MonadBaseControl IO m)
                   => ServerSettings -> Application m -> m ()
runTCPServerNoWait (ServerSettings port host) app = control $ \run -> bracket
    (liftIO $ CN.bindPort port host)
    (liftIO . NS.sClose)
    (run . forever . serve)
  where
    serve lsocket = do
        (socket, _addr) <- liftIO $ NS.accept lsocket
        liftIO $ NS.setSocketOption socket NS.NoDelay 1
        let src = sourceSocket socket
            sink = sinkSocket socket
            app' run = run (app src sink) >> return ()
            appClose run = app' run `finally` NS.sClose socket
        control $ \run -> forkIO (appClose run) >> run (return ())

-- | Run an @Application@ by connecting to the specified server.
-- Does TCP_NOWAIT
--
-- (stolen from network-conduit-0.5.2)
runTCPClientNoWait :: (MonadIO m, MonadBaseControl IO m)
                   => ClientSettings -> Application m -> m ()
runTCPClientNoWait (ClientSettings port host) app = control $ \run -> bracket
    (CN.getSocket host port)
    NS.sClose
    (\s ->
       do liftIO $ NS.setSocketOption s NS.NoDelay 1
          run $ app (sourceSocket s) (sinkSocket s))
