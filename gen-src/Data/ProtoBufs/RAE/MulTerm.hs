{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Data.ProtoBufs.RAE.MulTerm (MulTerm(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Data.ProtoBufs.RAE.OAFERefRadicals as RAE (OAFERefRadicals)
 
data MulTerm = MulTerm{left :: !RAE.OAFERefRadicals, right :: !RAE.OAFERefRadicals}
             deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable MulTerm where
  mergeAppend (MulTerm x'1 x'2) (MulTerm y'1 y'2) = MulTerm (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default MulTerm where
  defaultValue = MulTerm P'.defaultValue P'.defaultValue
 
instance P'.Wire MulTerm where
  wireSize ft' self'@(MulTerm x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 11 x'1 + P'.wireSizeReq 1 11 x'2)
  wirePut ft' self'@(MulTerm x'1 x'2)
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{left = P'.mergeAppend (left old'Self) (new'Field)}) (P'.wireGet 11)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{right = P'.mergeAppend (right old'Self) (new'Field)}) (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> MulTerm) MulTerm where
  getVal m' f' = f' m'
 
instance P'.GPB MulTerm
 
instance P'.ReflectDescriptor MulTerm where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10, 18]) (P'.fromDistinctAscList [10, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".RAE.MulTerm\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"RAE\"], baseName = MName \"MulTerm\"}, descFilePath = [\"Data\",\"ProtoBufs\",\"RAE\",\"MulTerm.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RAE.MulTerm.left\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"RAE\",MName \"MulTerm\"], baseName' = FName \"left\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RAE.OAFERefRadicals\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"RAE\"], baseName = MName \"OAFERefRadicals\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RAE.MulTerm.right\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"RAE\",MName \"MulTerm\"], baseName' = FName \"right\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RAE.OAFERefRadicals\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"RAE\"], baseName = MName \"OAFERefRadicals\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"