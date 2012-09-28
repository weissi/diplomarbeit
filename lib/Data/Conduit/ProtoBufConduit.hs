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

pbufSerialize :: (MonadResource m, ReflectDescriptor w, Wire w)
              => Conduit w m ByteString
pbufSerialize = awaitForever f
    where f pb = mapM_ yield $ BSL.toChunks $ runPut (messageWithLengthPutM pb)

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
