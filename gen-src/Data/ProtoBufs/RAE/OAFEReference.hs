{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Data.ProtoBufs.RAE.OAFEReference (OAFEReference(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data OAFEReference = OAFEReference{variable :: !P'.Utf8, index :: !P'.Int64}
                   deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable OAFEReference where
  mergeAppend (OAFEReference x'1 x'2) (OAFEReference y'1 y'2) = OAFEReference (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default OAFEReference where
  defaultValue = OAFEReference P'.defaultValue P'.defaultValue
 
instance P'.Wire OAFEReference where
  wireSize ft' self'@(OAFEReference x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 9 x'1 + P'.wireSizeReq 1 3 x'2)
  wirePut ft' self'@(OAFEReference x'1 x'2)
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
             P'.wirePutReq 16 3 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{variable = new'Field}) (P'.wireGet 9)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{index = new'Field}) (P'.wireGet 3)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> OAFEReference) OAFEReference where
  getVal m' f' = f' m'
 
instance P'.GPB OAFEReference
 
instance P'.ReflectDescriptor OAFEReference where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10, 16]) (P'.fromDistinctAscList [10, 16])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".RAE.OAFEReference\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"RAE\"], baseName = MName \"OAFEReference\"}, descFilePath = [\"Data\",\"ProtoBufs\",\"RAE\",\"OAFEReference.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RAE.OAFEReference.variable\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"RAE\",MName \"OAFEReference\"], baseName' = FName \"variable\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RAE.OAFEReference.index\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"RAE\",MName \"OAFEReference\"], baseName' = FName \"index\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"