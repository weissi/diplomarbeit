{-# LANGUAGE FlexibleContexts #-}
-- | Send all kinds of OAFE and RAE related data types over the network.
module Data.RAE.Conduit
    ( oafeConfigParseConduit
    , oafeConfigSerializeConduit
    , oafeEvaluationRequestParseConduit
    , oafeEvaluationResponseSerializeConduit
    , oafeEvaluationResponseParseConduit
    , oafeEvaluationRequestSerializeConduit
    , racFragmentSerializeConduit
    , racFragmentParseConduit
    , ByteSerializable(..)
    ) where

-- # STDLIB
import qualified Data.Foldable as F
import qualified Data.Sequence as S

-- # SITE PACKAGES
import Data.Conduit (Conduit, MonadResource, (=$=))
import Data.Text (Text)
import Data.Vector (Vector)
import Text.ProtocolBuffers.Basic (Utf8(Utf8))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Conduit.List as CL
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

-- # LOCAL
import Data.Conduit.ProtoBufConduit (pbufParse, pbufSerialize)
import Data.OAFE ( OAFEEvaluationRequest, OAFEEvaluationResponse
                 , OAFEReference(..)
                 )
import Data.RAE.Types (RAE(..), RACFragment)
import Data.FieldTypes (Field(..))
import Data.LinearExpression (VariableName)
import Math.FiniteFields.F2Pow256 (F2Pow256, f2Pow256FromBytes, f2Pow256ToBytes)

-- # PROTOBUF
import qualified Data.ProtoBufs.OAFE.LinearExpr as Pb
import qualified Data.ProtoBufs.OAFE.OAFEConfig as Pb
import qualified Data.ProtoBufs.OAFE.OAFEEvaluationRequest as Pb
import qualified Data.ProtoBufs.OAFE.OAFEEvaluationResponse as Pb
import qualified Data.ProtoBufs.RAE.RAE as PbRAE
import qualified Data.ProtoBufs.RAE.MulTerm as PbRAE
import qualified Data.ProtoBufs.RAE.OAFEReference as PbRAE

class ByteSerializable el where
    serializeBytes :: el -> BSL.ByteString
    parseBytes :: BSL.ByteString -> el

instance ByteSerializable F2Pow256 where
    serializeBytes = BSL.fromStrict . f2Pow256ToBytes
    parseBytes = f2Pow256FromBytes . BSL.toStrict

uToText :: Utf8 -> Text
uToText (Utf8 bs) = TE.decodeUtf8 $ BSL.toStrict bs

uFromText :: Text -> Utf8
uFromText = Utf8 . BSL.fromStrict . TE.encodeUtf8

-- # CONDUITS
oafeConfigParseConduit :: (MonadResource m, Field el, ByteSerializable el)
                       => Conduit BS.ByteString m
                                  (VariableName, Vector (el, el))
oafeConfigParseConduit = pbufParse =$= CL.map oafeConfigFromProtoBuf

oafeEvaluationRequestParseConduit :: ( MonadResource m, Field el
                                     , ByteSerializable el
                                     )
                                  => Conduit BS.ByteString m (VariableName, el)
oafeEvaluationRequestParseConduit =
    pbufParse =$= CL.map oafeEvaluationRequestFromProtoBuf

oafeConfigSerializeConduit :: (MonadResource m, Field el, ByteSerializable el)
                           => Conduit (VariableName, Vector (el, el)) m
                                      BS.ByteString
oafeConfigSerializeConduit = CL.map oafeConfigAsProtoBuf =$= pbufSerialize

oafeEvaluationResponseSerializeConduit :: ( MonadResource m, Field el
                                          , ByteSerializable el
                                          )
                                       => Conduit (OAFEEvaluationResponse el)
                                                  m BS.ByteString
oafeEvaluationResponseSerializeConduit =
    CL.map oafeEvaluationResponseAsProtoBuf =$= pbufSerialize

oafeEvaluationResponseParseConduit :: ( MonadResource m, Field el
                                      , ByteSerializable el
                                      )
                                   => Conduit BS.ByteString
                                              m (OAFEEvaluationResponse el)
oafeEvaluationResponseParseConduit =
    pbufParse =$= CL.map oafeEvaluationResponseFromProtoBuf

oafeEvaluationRequestSerializeConduit :: ( MonadResource m, Field el
                                         , ByteSerializable el
                                         )
                                      => Conduit (OAFEEvaluationRequest el)
                                                 m BS.ByteString
oafeEvaluationRequestSerializeConduit =
    CL.map oafeEvaluationRequestAsProtoBuf =$= pbufSerialize

racFragmentSerializeConduit :: (MonadResource m, Field el, ByteSerializable el)
                            => Conduit (RACFragment el) m BS.ByteString
racFragmentSerializeConduit = CL.map encodeRAE =$= pbufSerialize

racFragmentParseConduit :: (MonadResource m, Field el, ByteSerializable el)
                        => Conduit BS.ByteString m (RACFragment el)
racFragmentParseConduit = pbufParse =$= CL.map decodeRACFragment

-- # DE/ENCODING
oafeConfigFromProtoBuf :: (Field el, ByteSerializable el)
                       => Pb.OAFEConfig
                       -> (VariableName, Vector (el, el))
oafeConfigFromProtoBuf = decodeOAFEConfiguration

oafeEvaluationResponseAsProtoBuf :: (Field el, ByteSerializable el)
                                 => (VariableName, Vector el)
                                 -> Pb.OAFEEvaluationResponse
oafeEvaluationResponseAsProtoBuf (var, vals) =
    Pb.OAFEEvaluationResponse (uFromText var)
                              (S.fromList $ map serializeBytes $ V.toList vals)

oafeEvaluationRequestFromProtoBuf :: (Field el, ByteSerializable el)
                                  => Pb.OAFEEvaluationRequest
                                  -> (VariableName, el)
oafeEvaluationRequestFromProtoBuf (Pb.OAFEEvaluationRequest var val) =
    (uToText var, parseBytes val)

oafeEvaluationRequestAsProtoBuf :: (Field el, ByteSerializable el)
                                => OAFEEvaluationRequest el
                                -> Pb.OAFEEvaluationRequest
oafeEvaluationRequestAsProtoBuf (var, el) =
    Pb.OAFEEvaluationRequest (uFromText var) (serializeBytes el)

oafeEvaluationResponseFromProtoBuf :: (Field el, ByteSerializable el)
                                   => Pb.OAFEEvaluationResponse
                                   -> OAFEEvaluationResponse el
oafeEvaluationResponseFromProtoBuf (Pb.OAFEEvaluationResponse var vals) =
    (uToText var, V.fromList $ map parseBytes $ F.toList vals)

decodeOAFEConfiguration :: (Field el, ByteSerializable el)
                        => Pb.OAFEConfig
                        -> (VariableName, Vector (el, el))
decodeOAFEConfiguration (Pb.OAFEConfig var exprs) =
    (uToText var, V.map decodeLinearExpr $ (V.fromList . F.toList) exprs)

oafeConfigAsProtoBuf :: (Field el, ByteSerializable el)
                     => (VariableName, Vector (el, el))
                     -> Pb.OAFEConfig
oafeConfigAsProtoBuf (v, les) =
    Pb.OAFEConfig (uFromText v) (S.fromList $ V.toList $
                                   V.map encodeLinearExpr les)

encodeLinearExpr :: (Field el, ByteSerializable el) => (el, el) -> Pb.LinearExpr
encodeLinearExpr (s, i) =
    let enc = serializeBytes
     in Pb.LinearExpr (enc s) (enc i)

decodeLinearExpr :: (Field el, ByteSerializable el) => Pb.LinearExpr -> (el, el)
decodeLinearExpr lePb =
    let dec = parseBytes
     in (dec $ Pb.scale lePb, dec $ Pb.intercept lePb)

encodeOAFERef :: OAFEReference -> PbRAE.OAFEReference
encodeOAFERef (OAFERef var idx) =
   PbRAE.OAFEReference (uFromText var) (fromIntegral idx)

decodeOAFERef :: PbRAE.OAFEReference -> OAFEReference
decodeOAFERef (PbRAE.OAFEReference var idx) =
    OAFERef (uToText var) (fromIntegral idx)

encodeMulTerm :: (OAFEReference, OAFEReference) -> PbRAE.MulTerm
encodeMulTerm (l, r) = PbRAE.MulTerm (encodeOAFERef l) (encodeOAFERef r)

decodeMulTerm :: PbRAE.MulTerm -> (OAFEReference, OAFEReference)
decodeMulTerm (PbRAE.MulTerm l r) = (decodeOAFERef l, decodeOAFERef r)

encodeRAE :: ByteSerializable el => RACFragment el -> PbRAE.RAE
encodeRAE (var, RAE muls adds cnst) =
    let encConst = serializeBytes cnst
        encAdds = S.fromList $ map encodeOAFERef adds
        encMuls = S.fromList $ map encodeMulTerm muls
     in PbRAE.RAE (uFromText var) encMuls encAdds encConst

decodeRACFragment :: (Field el, ByteSerializable el)
                  => PbRAE.RAE -> RACFragment el
decodeRACFragment (PbRAE.RAE encVar encMuls encAdds encConst) =
    let var = uToText encVar
        muls = map decodeMulTerm $ F.toList encMuls
        adds = map decodeOAFERef $ F.toList encAdds
        cnst = parseBytes encConst
     in (var, RAE muls adds cnst)
