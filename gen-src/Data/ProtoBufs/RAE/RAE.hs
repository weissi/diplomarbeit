{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Data.ProtoBufs.RAE.RAE (RAE(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Data.ProtoBufs.RAE.MulTerm as RAE (MulTerm)
import qualified Data.ProtoBufs.RAE.OAFEReference as RAE (OAFEReference)
 
data RAE = RAE{variable :: !P'.Utf8, muls :: !(P'.Seq RAE.MulTerm), adds :: !(P'.Seq RAE.OAFEReference), const :: !P'.ByteString}
         deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable RAE where
  mergeAppend (RAE x'1 x'2 x'3 x'4) (RAE y'1 y'2 y'3 y'4)
   = RAE (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
 
instance P'.Default RAE where
  defaultValue = RAE P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire RAE where
  wireSize ft' self'@(RAE x'1 x'2 x'3 x'4)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeReq 1 9 x'1 + P'.wireSizeRep 1 11 x'2 + P'.wireSizeRep 1 11 x'3 + P'.wireSizeReq 1 12 x'4)
  wirePut ft' self'@(RAE x'1 x'2 x'3 x'4)
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
             P'.wirePutRep 26 11 x'3
             P'.wirePutReq 34 12 x'4
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{variable = new'Field}) (P'.wireGet 9)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{muls = P'.append (muls old'Self) new'Field}) (P'.wireGet 11)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{adds = P'.append (adds old'Self) new'Field}) (P'.wireGet 11)
             34 -> Prelude'.fmap (\ !new'Field -> old'Self{const = new'Field}) (P'.wireGet 12)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> RAE) RAE where
  getVal m' f' = f' m'
 
instance P'.GPB RAE
 
instance P'.ReflectDescriptor RAE where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList [10, 34]) (P'.fromDistinctAscList [10, 18, 26, 34])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".RAE.RAE\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"RAE\"], baseName = MName \"RAE\"}, descFilePath = [\"Data\",\"ProtoBufs\",\"RAE\",\"RAE.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RAE.RAE.variable\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"RAE\",MName \"RAE\"], baseName' = FName \"variable\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RAE.RAE.muls\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"RAE\",MName \"RAE\"], baseName' = FName \"muls\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RAE.MulTerm\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"RAE\"], baseName = MName \"MulTerm\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RAE.RAE.adds\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"RAE\",MName \"RAE\"], baseName' = FName \"adds\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RAE.OAFEReference\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"RAE\"], baseName = MName \"OAFEReference\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RAE.RAE.const\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"RAE\",MName \"RAE\"], baseName' = FName \"const\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"