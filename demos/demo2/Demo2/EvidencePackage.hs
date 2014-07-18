{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Demo2.EvidencePackage (EvidencePackage(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Demo2.Evidence as Demo2 (Evidence)
import qualified Demo2.Nonce as Demo2 (Nonce)
import qualified Demo2.Signature as Demo2 (Signature)
 
data EvidencePackage = EvidencePackage{evidence :: !(P'.Maybe Demo2.Evidence), nonce :: !(P'.Maybe Demo2.Nonce),
                                       signature :: !(P'.Maybe Demo2.Signature)}
                     deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable EvidencePackage where
  mergeAppend (EvidencePackage x'1 x'2 x'3) (EvidencePackage y'1 y'2 y'3)
   = EvidencePackage (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default EvidencePackage where
  defaultValue = EvidencePackage P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire EvidencePackage where
  wireSize ft' self'@(EvidencePackage x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 11 x'1 + P'.wireSizeOpt 1 11 x'2 + P'.wireSizeOpt 1 11 x'3)
  wirePut ft' self'@(EvidencePackage x'1 x'2 x'3)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 10 11 x'1
             P'.wirePutOpt 18 11 x'2
             P'.wirePutOpt 26 11 x'3
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{evidence = P'.mergeAppend (evidence old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{nonce = P'.mergeAppend (nonce old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             26 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{signature = P'.mergeAppend (signature old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> EvidencePackage) EvidencePackage where
  getVal m' f' = f' m'
 
instance P'.GPB EvidencePackage
 
instance P'.ReflectDescriptor EvidencePackage where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Demo2.EvidencePackage\", haskellPrefix = [], parentModule = [MName \"Demo2\"], baseName = MName \"EvidencePackage\"}, descFilePath = [\"Demo2\",\"EvidencePackage.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Demo2.EvidencePackage.evidence\", haskellPrefix' = [], parentModule' = [MName \"Demo2\",MName \"EvidencePackage\"], baseName' = FName \"evidence\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Demo2.Evidence\", haskellPrefix = [], parentModule = [MName \"Demo2\"], baseName = MName \"Evidence\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Demo2.EvidencePackage.nonce\", haskellPrefix' = [], parentModule' = [MName \"Demo2\",MName \"EvidencePackage\"], baseName' = FName \"nonce\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Demo2.Nonce\", haskellPrefix = [], parentModule = [MName \"Demo2\"], baseName = MName \"Nonce\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Demo2.EvidencePackage.signature\", haskellPrefix' = [], parentModule' = [MName \"Demo2\",MName \"EvidencePackage\"], baseName' = FName \"signature\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Demo2.Signature\", haskellPrefix = [], parentModule = [MName \"Demo2\"], baseName = MName \"Signature\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"