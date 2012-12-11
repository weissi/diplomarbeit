{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Data.ProtoBufs.Setup.SetupGoliathToDavid (SetupGoliathToDavid(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data SetupGoliathToDavid = SetupGoliathToDavid{tokenHost :: !P'.Utf8, tokenPort :: !P'.Int32}
                         deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable SetupGoliathToDavid where
  mergeAppend (SetupGoliathToDavid x'1 x'2) (SetupGoliathToDavid y'1 y'2)
   = SetupGoliathToDavid (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default SetupGoliathToDavid where
  defaultValue = SetupGoliathToDavid P'.defaultValue P'.defaultValue
 
instance P'.Wire SetupGoliathToDavid where
  wireSize ft' self'@(SetupGoliathToDavid x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 9 x'1 + P'.wireSizeReq 1 5 x'2)
  wirePut ft' self'@(SetupGoliathToDavid x'1 x'2)
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{tokenHost = new'Field}) (P'.wireGet 9)
             16 -> Prelude'.fmap (\ !new'Field -> old'Self{tokenPort = new'Field}) (P'.wireGet 5)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> SetupGoliathToDavid) SetupGoliathToDavid where
  getVal m' f' = f' m'
 
instance P'.GPB SetupGoliathToDavid
 
instance P'.ReflectDescriptor SetupGoliathToDavid where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10, 16]) (P'.fromDistinctAscList [10, 16])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Setup.SetupGoliathToDavid\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"Setup\"], baseName = MName \"SetupGoliathToDavid\"}, descFilePath = [\"Data\",\"ProtoBufs\",\"Setup\",\"SetupGoliathToDavid.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Setup.SetupGoliathToDavid.tokenHost\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"Setup\",MName \"SetupGoliathToDavid\"], baseName' = FName \"tokenHost\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Setup.SetupGoliathToDavid.tokenPort\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"Setup\",MName \"SetupGoliathToDavid\"], baseName' = FName \"tokenPort\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"