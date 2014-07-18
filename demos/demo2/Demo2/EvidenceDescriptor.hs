{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Demo2.EvidenceDescriptor (EvidenceDescriptor(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data EvidenceDescriptor = EvidenceDescriptor{desc :: !(P'.Maybe P'.Int32)}
                        deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable EvidenceDescriptor where
  mergeAppend (EvidenceDescriptor x'1) (EvidenceDescriptor y'1) = EvidenceDescriptor (P'.mergeAppend x'1 y'1)
 
instance P'.Default EvidenceDescriptor where
  defaultValue = EvidenceDescriptor P'.defaultValue
 
instance P'.Wire EvidenceDescriptor where
  wireSize ft' self'@(EvidenceDescriptor x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 5 x'1)
  wirePut ft' self'@(EvidenceDescriptor x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutOpt 8 5 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             8 -> Prelude'.fmap (\ !new'Field -> old'Self{desc = Prelude'.Just new'Field}) (P'.wireGet 5)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> EvidenceDescriptor) EvidenceDescriptor where
  getVal m' f' = f' m'
 
instance P'.GPB EvidenceDescriptor
 
instance P'.ReflectDescriptor EvidenceDescriptor where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [8])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Demo2.EvidenceDescriptor\", haskellPrefix = [], parentModule = [MName \"Demo2\"], baseName = MName \"EvidenceDescriptor\"}, descFilePath = [\"Demo2\",\"EvidenceDescriptor.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Demo2.EvidenceDescriptor.desc\", haskellPrefix' = [], parentModule' = [MName \"Demo2\",MName \"EvidenceDescriptor\"], baseName' = FName \"desc\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 8}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 5}, typeName = Nothing, hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"