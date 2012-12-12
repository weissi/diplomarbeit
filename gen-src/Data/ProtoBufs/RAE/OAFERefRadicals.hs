{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Data.ProtoBufs.RAE.OAFERefRadicals (OAFERefRadicals(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Data.ProtoBufs.RAE.OAFEReference as RAE (OAFEReference)
 
data OAFERefRadicals = OAFERefRadicals{one :: !RAE.OAFEReference, two :: !RAE.OAFEReference}
                     deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable OAFERefRadicals where
  mergeAppend (OAFERefRadicals x'1 x'2) (OAFERefRadicals y'1 y'2)
   = OAFERefRadicals (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default OAFERefRadicals where
  defaultValue = OAFERefRadicals P'.defaultValue P'.defaultValue
 
instance P'.Wire OAFERefRadicals where
  wireSize ft' self'@(OAFERefRadicals x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 11 x'1 + P'.wireSizeReq 1 11 x'2)
  wirePut ft' self'@(OAFERefRadicals x'1 x'2)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutReq 10 11 x'1
             P'.wirePutReq 18 11 x'2
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{one = P'.mergeAppend (one old'Self) (new'Field)}) (P'.wireGet 11)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{two = P'.mergeAppend (two old'Self) (new'Field)}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> OAFERefRadicals) OAFERefRadicals where
  getVal m' f' = f' m'
 
instance P'.GPB OAFERefRadicals
 
instance P'.ReflectDescriptor OAFERefRadicals where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10, 18]) (P'.fromDistinctAscList [10, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".RAE.OAFERefRadicals\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"RAE\"], baseName = MName \"OAFERefRadicals\"}, descFilePath = [\"Data\",\"ProtoBufs\",\"RAE\",\"OAFERefRadicals.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RAE.OAFERefRadicals.one\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"RAE\",MName \"OAFERefRadicals\"], baseName' = FName \"one\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RAE.OAFEReference\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"RAE\"], baseName = MName \"OAFEReference\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RAE.OAFERefRadicals.two\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"RAE\",MName \"OAFERefRadicals\"], baseName' = FName \"two\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RAE.OAFEReference\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"RAE\"], baseName = MName \"OAFEReference\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"