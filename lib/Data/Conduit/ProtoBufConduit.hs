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

-- | Bring together Conduits and ProtocolBuffers.
module Data.Conduit.ProtoBufConduit (pbufSerialize, pbufParse) where

-- # STDLIB
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL

-- # SITE PACKAGES
import Data.Conduit (Conduit, MonadResource, yield, awaitForever, await)
import Text.ProtocolBuffers.Get (Result(..))
import Text.ProtocolBuffers.Reflections (ReflectDescriptor)
import Text.ProtocolBuffers.WireMessage ( Wire
                                        , messageWithLengthPutM, runPut
                                        , messageWithLengthGetM, runGet
                                        )

-- | Serialize to a Protocol Buffer (as @ByteString@).
pbufSerialize :: (MonadResource m, ReflectDescriptor w, Wire w)
              => Conduit w m ByteString
pbufSerialize = awaitForever f
    where f pb = mapM_ yield $ BSL.toChunks $ runPut (messageWithLengthPutM pb)

-- | Parse a ProtocolBuffer (@ByteString@) to some data structure.
pbufParse :: (MonadResource m, ReflectDescriptor w, Wire w, Show w)
          => Conduit ByteString m w
pbufParse =
    let new = readPbuf (runGet messageWithLengthGetM . BSL.fromChunks . (:[]))
        readPbuf parse =
            do mbs <- await
               case mbs of
                 Just bs -> checkResult (parse bs)
                 Nothing -> return ()
        checkResult result =
            case result of
              Failed _ errmsg -> fail errmsg
              Partial cont -> readPbuf (cont . Just . BSL.fromChunks . (:[]))
              Finished rest _ msg ->
                  do yield msg
                     checkResult (runGet messageWithLengthGetM rest)
    in new
