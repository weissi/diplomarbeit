{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Helpers ( integralBytes, takeOneConduit
                    , runTCPClientNoWait
                    , isOptionArg
                    ) where
import Blaze.ByteString.Builder (Builder, toByteString)
import Blaze.ByteString.Builder.Char8 (fromChar)
import Control.Exception (bracket)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl, control)
import Data.Bits (Bits, shiftR)
import Data.ByteString
import Data.Char (chr)
import Data.Conduit.Network ( ClientSettings(..), Application
                            , sourceSocket, sinkSocket
                            )
import Data.Conduit.Network.Internal (AppData(..))
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

-- | Run an @Application@ by connecting to the specified server.
-- Does TCP_NOWAIT
--
-- (stolen from network-conduit-0.6.1.1)
runTCPClientNoWait :: (MonadIO m, MonadBaseControl IO m)
                   => ClientSettings m -> Application m -> m ()
runTCPClientNoWait cs app =
    let port = clientPort cs
        host = clientHost cs
     in control $ \run -> bracket
        (CN.getSocket host port)
        (NS.sClose . fst)
        (\(s, address) ->
            do liftIO $ NS.setSocketOption s NS.NoDelay 1
               run $ app AppData
                         { appSource = sourceSocket s
                         , appSink = sinkSocket s
                         , appSockAddr = address
                         , appLocalAddr = Nothing
                         })

isOptionArg :: String -> Bool
isOptionArg s =
    case s of
      [] -> False
      ('-':_) -> True
      _ -> False
