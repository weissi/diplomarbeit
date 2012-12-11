{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Data.ProtoBufs.Setup.SetupDavidToGoliath (SetupDavidToGoliath(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data SetupDavidToGoliath = SetupDavidToGoliath{host :: !P'.Utf8, port :: !P'.Int32}
                         deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable SetupDavidToGoliath where
  mergeAppend (SetupDavidToGoliath x'1 x'2) (SetupDavidToGoliath y'1 y'2)
   = SetupDavidToGoliath (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default SetupDavidToGoliath where
  defaultValue = SetupDavidToGoliath P'.defaultValue P'.defaultValue
 
instance P'.Wire SetupDavidToGoliath where
  wireSize ft' self'@(SetupDavidToGoliath x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 9 x'1 + P'.wireSizeReq 1 5 x'2)
  wirePut ft' self'@(SetupDavidToGoliath x'1 x'2)
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
             P'.wirePutReq 16 5 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{host = new'Field}) (P'.wireGet 9)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{port = new'Field}) (P'.wireGet 5)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> SetupDavidToGoliath) SetupDavidToGoliath where
  getVal m' f' = f' m'
 
instance P'.GPB SetupDavidToGoliath
 
instance P'.ReflectDescriptor SetupDavidToGoliath where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10, 16]) (P'.fromDistinctAscList [10, 16])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Setup.SetupDavidToGoliath\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"Setup\"], baseName = MName \"SetupDavidToGoliath\"}, descFilePath = [\"Data\",\"ProtoBufs\",\"Setup\",\"SetupDavidToGoliath.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Setup.SetupDavidToGoliath.host\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"Setup\",MName \"SetupDavidToGoliath\"], baseName' = FName \"host\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Setup.SetupDavidToGoliath.port\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"Setup\",MName \"SetupDavidToGoliath\"], baseName' = FName \"port\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"