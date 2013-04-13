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
import Data.ByteString (ByteString)
import Data.Char (chr)
import Data.Conduit.Network ( ClientSettings(..), Application
                            , sourceSocket, sinkSocket
                            )
import Data.Conduit.Network.Internal (AppData(..))
import Data.Maybe (fromJust)
import Data.Monoid (mempty, mappend)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Network as CN
import qualified Network.Socket as NS

-- | Encode a value of an 'Integral' type as 'ByteString'.
integralBytes :: (Integral a, Bits a, Show a)
              => a           -- ^ Value to encode
              -> ByteString  -- ^ Returned 'ByteString'
integralBytes n0
  | n0 <  0   = error ("integralBytes: applied to negative number " ++ show n0)
  | otherwise =
      let !bs = toByteString $ marshallIntByte n0 mempty
       in bs
  where marshallIntByte :: (Show a, Bits a, Integral a)
                        => a -> Builder -> Builder
        marshallIntByte !n !acc =
           let !newChar = chr . fromIntegral $ n `mod` 256
               !newBuilder = fromChar newChar
            in case n of
                 0 -> acc
                 _ -> marshallIntByte (n `shiftR` 8) (acc `mappend` newBuilder)

-- | A 'C.Conduit' drops every element but the first.
takeOneConduit :: C.MonadResource m => C.Conduit i m i
takeOneConduit =
    do mX <- CL.head
       C.yield $ fromJust mX

-- | Run an 'Application' by connecting to the specified server.
-- Does TCP_NOWAIT
--
-- (stolen from network-conduit-0.6.1.1)
runTCPClientNoWait :: (MonadIO m, MonadBaseControl IO m)
                   => ClientSettings m      -- ^ Settings such as host and port.
                   -> Application m -> m ()
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

-- | Predicate to decide whether a command line argument is an optional argument
-- (starts with a dash).
isOptionArg :: String -> Bool
isOptionArg s =
    case s of
      [] -> False
      ('-':_) -> True
      _ -> False
