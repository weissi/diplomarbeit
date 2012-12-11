{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Data.ProtoBufs.RAE (protoInfo, fileDescriptorProto) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import Text.DescriptorProtos.FileDescriptorProto (FileDescriptorProto)
import Text.ProtocolBuffers.Reflections (ProtoInfo)
import qualified Text.ProtocolBuffers.WireMessage as P' (wireGet,getFromBS)
 
protoInfo :: ProtoInfo
protoInfo
 = Prelude'.read
    "ProtoInfo {protoMod = ProtoName {protobufName = FIName \".RAE\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [], baseName = MName \"RAE\"}, protoFilePath = [\"Data\",\"ProtoBufs\",\"RAE.hs\"], protoSource = \"RAE.proto\", extensionKeys = fromList [], messages = [DescriptorInfo {descName = ProtoName {protobufName = FIName \".RAE.RAE\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"RAE\"], baseName = MName \"RAE\"}, descFilePath = [\"Data\",\"ProtoBufs\",\"RAE\",\"RAE.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RAE.RAE.variable\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"RAE\",MName \"RAE\"], baseName' = FName \"variable\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RAE.RAE.muls\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"RAE\",MName \"RAE\"], baseName' = FName \"muls\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RAE.MulTerm\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"RAE\"], baseName = MName \"MulTerm\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RAE.RAE.adds\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"RAE\",MName \"RAE\"], baseName' = FName \"adds\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RAE.OAFEReference\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"RAE\"], baseName = MName \"OAFEReference\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RAE.RAE.const\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"RAE\",MName \"RAE\"], baseName' = FName \"const\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 34}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False},DescriptorInfo {descName = ProtoName {protobufName = FIName \".RAE.MulTerm\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"RAE\"], baseName = MName \"MulTerm\"}, descFilePath = [\"Data\",\"ProtoBufs\",\"RAE\",\"MulTerm.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RAE.MulTerm.left\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"RAE\",MName \"MulTerm\"], baseName' = FName \"left\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RAE.OAFEReference\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"RAE\"], baseName = MName \"OAFEReference\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RAE.MulTerm.right\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"RAE\",MName \"MulTerm\"], baseName' = FName \"right\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".RAE.OAFEReference\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"RAE\"], baseName = MName \"OAFEReference\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False},DescriptorInfo {descName = ProtoName {protobufName = FIName \".RAE.OAFEReference\", haskellPrefix = [MName \"Data\",MName \"ProtoBufs\"], parentModule = [MName \"RAE\"], baseName = MName \"OAFEReference\"}, descFilePath = [\"Data\",\"ProtoBufs\",\"RAE\",\"OAFEReference.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RAE.OAFEReference.variable\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"RAE\",MName \"OAFEReference\"], baseName' = FName \"variable\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".RAE.OAFEReference.index\", haskellPrefix' = [MName \"Data\",MName \"ProtoBufs\"], parentModule' = [MName \"RAE\",MName \"OAFEReference\"], baseName' = FName \"index\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 16}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = True, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 3}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}], enums = [], knownKeyMap = fromList []}"
 
fileDescriptorProto :: FileDescriptorProto
fileDescriptorProto
 = P'.getFromBS (P'.wireGet 11)
    (P'.pack
      "\255\SOH\n\tRAE.proto\SUB\nOAFE.proto\"d\n\ETXRAE\DC2\DLE\n\bvariable\CAN\SOH \STX(\t\DC2\SUB\n\EOTmuls\CAN\STX \ETX(\v2\f.RAE.MulTerm\DC2 \n\EOTadds\CAN\ETX \ETX(\v2\DC2.RAE.OAFEReference\DC2\r\n\ENQconst\CAN\EOT \STX(\f\"N\n\aMulTerm\DC2 \n\EOTleft\CAN\SOH \STX(\v2\DC2.RAE.OAFEReference\DC2!\n\ENQright\CAN\STX \STX(\v2\DC2.RAE.OAFEReference\"0\n\rOAFEReference\DC2\DLE\n\bvariable\CAN\SOH \STX(\t\DC2\r\n\ENQindex\CAN\STX \STX(\ETX")