{-# LANGUAGE FlexibleContexts #-}
module Data.OAFEComm ( oafeConfigAsProtoBuf
                     , oafeConfigFromProtoBuf
                     , oafeConfigParseConduit
                     , oafeConfigSerializeConduit
                     , oafeEvaluationRequestAsProtoBuf
                     , oafeEvaluationRequestParseConduit
                     , oafeEvaluationResponseAsProtoBuf
                     , oafeEvaluationResponseFromProtoBuf
                     , oafeEvaluationResponseSerializeConduit
                     , oafeEvaluationResponseParseConduit
                     , oafeEvaluationRequestSerializeConduit
                     , areSerializeConduit
                     , areParseConduit
                     )
                     where

-- # STDLIB
import qualified Data.Foldable as F
import qualified Data.Map as M
import qualified Data.Sequence as S

-- # SITE PACKAGES
import Data.Conduit (Conduit, MonadResource, (=$=))
import Text.ProtocolBuffers.Basic (uFromString, uToString)
import qualified Data.ByteString as BS
import qualified Data.Conduit.List as CL

-- # LOCAL
import Data.Conduit.ProtoBufConduit (pbufParse, pbufSerialize)
import Data.DAREEvaluation ( EDARE(..), OAFEConfiguration, OAFEReference(..)
                           , OAFEEvaluationRequest, OAFEEvaluationResponse
                           )
import Data.FieldTypes (Field(..))
import Data.LinearExpression (VariableName)

-- # PROTOBUF
import qualified Data.ProtoBufs.OAFE.OAFEConfigSet as Pb
import qualified Data.ProtoBufs.OAFE.LinearExpr as Pb
import qualified Data.ProtoBufs.OAFE.OAFEConfig as Pb
import qualified Data.ProtoBufs.OAFE.OAFEEvaluationRequest as Pb
import qualified Data.ProtoBufs.OAFE.OAFEEvaluationResponse as Pb
import qualified Data.ProtoBufs.ARE.ARE as PbARE
import qualified Data.ProtoBufs.ARE.MulTerm as PbARE
import qualified Data.ProtoBufs.ARE.OAFEReference as PbARE


-- # CONDUITS
oafeConfigParseConduit :: (MonadResource m, Field el, Read el)
                       => Conduit BS.ByteString m (OAFEConfiguration el)
oafeConfigParseConduit = pbufParse =$= CL.map oafeConfigFromProtoBuf

oafeEvaluationRequestParseConduit :: (MonadResource m, Field el, Read el)
                                  => Conduit BS.ByteString m (VariableName, el)
oafeEvaluationRequestParseConduit =
    pbufParse =$= CL.map oafeEvaluationRequestFromProtoBuf

oafeConfigSerializeConduit :: (MonadResource m, Field el, Show el)
                           => Conduit (OAFEConfiguration el) m BS.ByteString
oafeConfigSerializeConduit = CL.map oafeConfigAsProtoBuf =$= pbufSerialize

oafeEvaluationResponseSerializeConduit :: (MonadResource m, Field el, Show el)
                                       => Conduit (OAFEEvaluationResponse el)
                                                  m BS.ByteString
oafeEvaluationResponseSerializeConduit =
    CL.map oafeEvaluationResponseAsProtoBuf =$= pbufSerialize

oafeEvaluationResponseParseConduit :: (MonadResource m, Field el, Read el)
                                   => Conduit BS.ByteString
                                              m (OAFEEvaluationResponse el)
oafeEvaluationResponseParseConduit =
    pbufParse =$= CL.map oafeEvaluationResponseFromProtoBuf

oafeEvaluationRequestSerializeConduit :: (MonadResource m, Field el, Show el)
                                      => Conduit (OAFEEvaluationRequest el)
                                                 m BS.ByteString
oafeEvaluationRequestSerializeConduit =
    CL.map oafeEvaluationRequestAsProtoBuf =$= pbufSerialize

areSerializeConduit :: (MonadResource m, Field el, Show el)
                    => Conduit (VariableName, EDARE OAFEReference el)
                               m
                               BS.ByteString
areSerializeConduit =
    CL.map encodeARE =$= pbufSerialize

areParseConduit :: (MonadResource m, Field el, Read el)
                => Conduit BS.ByteString
                           m
                           (VariableName, EDARE OAFEReference el)
areParseConduit =
    pbufParse =$= CL.map decodeARE

-- # DE/ENCODING
oafeConfigAsProtoBuf :: (Field el, Show el)
                     => OAFEConfiguration el
                     -> Pb.OAFEConfigSet
oafeConfigAsProtoBuf oac =
    let oacs = map (uncurry encodeOAFEConfiguration) (M.toList oac)
     in Pb.OAFEConfigSet { Pb.configs = S.fromList oacs }

oafeConfigFromProtoBuf :: (Field el, Read el)
                       => Pb.OAFEConfigSet
                       -> OAFEConfiguration el
oafeConfigFromProtoBuf = decodeOAFEConfiguration

oafeEvaluationResponseAsProtoBuf :: (Field el, Show el)
                                 => (VariableName, [el])
                                 -> Pb.OAFEEvaluationResponse
oafeEvaluationResponseAsProtoBuf (var, vals) =
    Pb.OAFEEvaluationResponse (uFromString var)
                              (S.fromList $ map (uFromString . show) vals)

oafeEvaluationRequestFromProtoBuf :: (Field el, Read el)
                                  => Pb.OAFEEvaluationRequest
                                  -> (VariableName, el)
oafeEvaluationRequestFromProtoBuf (Pb.OAFEEvaluationRequest var val) =
    (uToString var, (read . uToString) val)

oafeEvaluationRequestAsProtoBuf :: (Field el, Show el)
                                => OAFEEvaluationRequest el
                                -> Pb.OAFEEvaluationRequest
oafeEvaluationRequestAsProtoBuf (var, el) =
    Pb.OAFEEvaluationRequest (uFromString var) ((uFromString . show) el)

oafeEvaluationResponseFromProtoBuf :: (Field el, Read el)
                                   => Pb.OAFEEvaluationResponse
                                   -> OAFEEvaluationResponse el
oafeEvaluationResponseFromProtoBuf (Pb.OAFEEvaluationResponse var vals) =
    (uToString var, map (read . uToString) $ F.toList vals)

decodeOAFEConfiguration :: (Read el, Field el)
                        => Pb.OAFEConfigSet
                        -> OAFEConfiguration el
decodeOAFEConfiguration oacs =
    let dec (Pb.OAFEConfig var exprs) =
            (uToString var, map decodeLinearExpr $ F.toList exprs)
     in M.fromList $ map dec (F.toList (Pb.configs oacs))

encodeOAFEConfiguration :: (Show el, Field el)
                        => VariableName
                        -> [(el, el)]
                        -> Pb.OAFEConfig
encodeOAFEConfiguration v les =
    Pb.OAFEConfig (uFromString v) (S.fromList $ map encodeLinearExpr les)

encodeLinearExpr :: (Show el, Field el) => (el, el) -> Pb.LinearExpr
encodeLinearExpr (s, i) =
    let enc = uFromString . show
     in Pb.LinearExpr (enc s) (enc i)

decodeLinearExpr :: (Read el, Field el) => Pb.LinearExpr -> (el, el)
decodeLinearExpr lePb =
    let dec = read . uToString
     in (dec $ Pb.scale lePb, dec $ Pb.intercept lePb)

encodeOAFERef :: OAFEReference -> PbARE.OAFEReference
encodeOAFERef (OAFERef var idx) =
   PbARE.OAFEReference (uFromString var) (fromIntegral idx)

decodeOAFERef :: PbARE.OAFEReference -> OAFEReference
decodeOAFERef (PbARE.OAFEReference var idx) =
    OAFERef (uToString var) (fromIntegral idx)

encodeMulTerm :: (OAFEReference, OAFEReference) -> PbARE.MulTerm
encodeMulTerm (l, r) = PbARE.MulTerm (encodeOAFERef l) (encodeOAFERef r)

decodeMulTerm :: PbARE.MulTerm -> (OAFEReference, OAFEReference)
decodeMulTerm (PbARE.MulTerm l r) = (decodeOAFERef l, decodeOAFERef r)

encodeARE :: Show el
          => (VariableName, EDARE OAFEReference el)
          -> PbARE.ARE
encodeARE (var, EDARE muls adds cnst) =
    let encConst = (uFromString . show) cnst
        encAdds = S.fromList $ map encodeOAFERef adds
        encMuls = S.fromList $ map encodeMulTerm muls
     in PbARE.ARE (uFromString var) encMuls encAdds encConst

decodeARE :: (Field el, Read el)
          => PbARE.ARE
          -> (VariableName, EDARE OAFEReference el)
decodeARE (PbARE.ARE encVar encMuls encAdds encConst) =
    let var = uToString encVar
        muls = map decodeMulTerm $ F.toList encMuls
        adds = map decodeOAFERef $ F.toList encAdds
        cnst = (read . uToString) encConst
     in (var, EDARE muls adds cnst)
