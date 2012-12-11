{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Data.ProtoBufs.OAFE.OAFEConfig (OAFEConfig(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Data.ProtoBufs.OAFE.LinearExpr as OAFE (LinearExpr)
 
data OAFEConfig = OAFEConfig{variable :: !P'.Utf8, expressions :: !(P'.Seq OAFE.LinearExpr)}
                deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable OAFEConfig where
  mergeAppend (OAFEConfig x'1 x'2) (OAFEConfig y'1 y'2) = OAFEConfig (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default OAFEConfig where
  defaultValue = OAFEConfig P'.defaultValue P'.defaultValue
 
instance P'.Wire OAFEConfig where
  wireSize ft' self'@(OAFEConfig x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 9 x'1 + P'.wireSizeRep 1 11 x'2)
  wirePut ft' self'@(OAFEConfig x'1 x'2)
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
             P'.wirePutRep 18 11 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{variable = new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{expressions = P'.append (expressions old'Self) new'Field})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> OAFEConfig) OAFEConfig where
  getVal m' f' = f' m'
 
instance P'.GPB OAFEConfig
 
instance P'.ReflectDescriptor OAFEConfig where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10]) (P'.fromDistinctAscList [10, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".OAFE.OAFEConfig\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"OAFE\"], baseName = MName \"OAFEConfig\"}, descFilePath = [\"Data\",\"ProtoBufs\",\"OAFE\",\"OAFEConfig.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".OAFE.OAFEConfig.variable\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"OAFE\",MName \"OAFEConfig\"], baseName' = FName \"variable\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".OAFE.OAFEConfig.expressions\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"OAFE\",MName \"OAFEConfig\"], baseName' = FName \"expressions\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".OAFE.LinearExpr\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"OAFE\"], baseName = MName \"LinearExpr\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"