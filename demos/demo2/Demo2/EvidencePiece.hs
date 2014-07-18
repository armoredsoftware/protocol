{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Demo2.EvidencePiece (EvidencePiece(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Demo2.EvidencePiece.Constructor as Demo2.EvidencePiece (Constructor)
 
data EvidencePiece = EvidencePiece{constructor :: !(P'.Maybe Demo2.EvidencePiece.Constructor),
                                   bytesData :: !(P'.Maybe P'.ByteString), int32Data :: !(P'.Maybe P'.Int32),
                                   doubleData :: !(P'.Maybe P'.Double), boolData :: !(P'.Maybe P'.Bool),
                                   stringData :: !(P'.Maybe P'.Utf8)}
                   deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable EvidencePiece where
  mergeAppend (EvidencePiece x'1 x'2 x'3 x'4 x'5 x'6) (EvidencePiece y'1 y'2 y'3 y'4 y'5 y'6)
   = EvidencePiece (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3) (P'.mergeAppend x'4 y'4)
      (P'.mergeAppend x'5 y'5)
      (P'.mergeAppend x'6 y'6)
 
instance P'.Default EvidencePiece where
  defaultValue = EvidencePiece P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire EvidencePiece where
  wireSize ft' self'@(EvidencePiece x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size
         = (P'.wireSizeOpt 1 14 x'1 + P'.wireSizeOpt 1 12 x'2 + P'.wireSizeOpt 1 5 x'3 + P'.wireSizeOpt 1 1 x'4 +
             P'.wireSizeOpt 1 8 x'5
             + P'.wireSizeOpt 1 9 x'6)
  wirePut ft' self'@(EvidencePiece x'1 x'2 x'3 x'4 x'5 x'6)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 8 14 x'1
             P'.wirePutOpt 18 12 x'2
             P'.wirePutOpt 24 5 x'3
             P'.wirePutOpt 33 1 x'4
             P'.wirePutOpt 40 8 x'5
             P'.wirePutOpt 50 9 x'6
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{constructor = Prelude'.Just new'Field}) (P'.wireGet 14)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{bytesData = Prelude'.Just new'Field}) (P'.wireGet 12)
             24 -> Prelude'.fmap (\ !new'Field -> old'Self{int32Data = Prelude'.Just new'Field}) (P'.wireGet 5)
             33 -> Prelude'.fmap (\ !new'Field -> old'Self{doubleData = Prelude'.Just new'Field}) (P'.wireGet 1)
             40 -> Prelude'.fmap (\ !new'Field -> old'Self{boolData = Prelude'.Just new'Field}) (P'.wireGet 8)
             50 -> Prelude'.fmap (\ !new'Field -> old'Self{stringData = Prelude'.Just new'Field}) (P'.wireGet 9)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> EvidencePiece) EvidencePiece where
  getVal m' f' = f' m'
 
instance P'.GPB EvidencePiece
 
instance P'.ReflectDescriptor EvidencePiece where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8, 18, 24, 33, 40, 50])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Demo2.EvidencePiece\", haskellPrefix = [], parentModule = [MName \"Demo2\"], baseName = MName \"EvidencePiece\"}, descFilePath = [\"Demo2\",\"EvidencePiece.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Demo2.EvidencePiece.constructor\", haskellPrefix' = [], parentModule' = [MName \"Demo2\",MName \"EvidencePiece\"], baseName' = FName \"constructor\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 14}, typeName = Just (ProtoName {protobufName = FIName \".Demo2.EvidencePiece.Constructor\", haskellPrefix = [], parentModule = [MName \"Demo2\",MName \"EvidencePiece\"], baseName = MName \"Constructor\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Demo2.EvidencePiece.bytesData\", haskellPrefix' = [], parentModule' = [MName \"Demo2\",MName \"EvidencePiece\"], baseName' = FName \"bytesData\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 12}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Demo2.EvidencePiece.int32Data\", haskellPrefix' = [], parentModule' = [MName \"Demo2\",MName \"EvidencePiece\"], baseName' = FName \"int32Data\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 24}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Demo2.EvidencePiece.doubleData\", haskellPrefix' = [], parentModule' = [MName \"Demo2\",MName \"EvidencePiece\"], baseName' = FName \"doubleData\"}, fieldNumber = FieldId {getFieldId = 4}, wireTag = WireTag {getWireTag = 33}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 1}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Demo2.EvidencePiece.boolData\", haskellPrefix' = [], parentModule' = [MName \"Demo2\",MName \"EvidencePiece\"], baseName' = FName \"boolData\"}, fieldNumber = FieldId {getFieldId = 5}, wireTag = WireTag {getWireTag = 40}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 8}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Demo2.EvidencePiece.stringData\", haskellPrefix' = [], parentModule' = [MName \"Demo2\",MName \"EvidencePiece\"], baseName' = FName \"stringData\"}, fieldNumber = FieldId {getFieldId = 6}, wireTag = WireTag {getWireTag = 50}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 9}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"