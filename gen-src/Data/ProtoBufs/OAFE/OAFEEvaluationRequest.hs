{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Data.ProtoBufs.OAFE.OAFEEvaluationRequest (OAFEEvaluationRequest(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data OAFEEvaluationRequest = OAFEEvaluationRequest{variable :: !P'.Utf8, value :: !P'.ByteString}
                           deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable OAFEEvaluationRequest where
  mergeAppend (OAFEEvaluationRequest x'1 x'2) (OAFEEvaluationRequest y'1 y'2)
   = OAFEEvaluationRequest (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default OAFEEvaluationRequest where
  defaultValue = OAFEEvaluationRequest P'.defaultValue P'.defaultValue
 
instance P'.Wire OAFEEvaluationRequest where
  wireSize ft' self'@(OAFEEvaluationRequest x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 9 x'1 + P'.wireSizeReq 1 12 x'2)
  wirePut ft' self'@(OAFEEvaluationRequest x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 10 9 x'1
             P'.wirePutReq 18 12 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{variable = new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{value = new'Field}) (P'.wireGet 12)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> OAFEEvaluationRequest) OAFEEvaluationRequest where
  getVal m' f' = f' m'
 
instance P'.GPB OAFEEvaluationRequest
 
instance P'.ReflectDescriptor OAFEEvaluationRequest where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10, 18]) (P'.fromDistinctAscList [10, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".OAFE.OAFEEvaluationRequest\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"OAFE\"], baseName = MName \"OAFEEvaluationRequest\"}, descFilePath = [\"Data\",\"ProtoBufs\",\"OAFE\",\"OAFEEvaluationRequest.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".OAFE.OAFEEvaluationRequest.variable\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"OAFE\",MName \"OAFEEvaluationRequest\"], baseName' = FName \"variable\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".OAFE.OAFEEvaluationRequest.value\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"OAFE\",MName \"OAFEEvaluationRequest\"], baseName' = FName \"value\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"