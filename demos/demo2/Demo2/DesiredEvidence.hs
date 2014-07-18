{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Demo2.DesiredEvidence (DesiredEvidence(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Demo2.EvidenceDescriptor as Demo2 (EvidenceDescriptor)
 
data DesiredEvidence = DesiredEvidence{evidenceDescriptor :: !(P'.Seq Demo2.EvidenceDescriptor)}
                     deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable DesiredEvidence where
  mergeAppend (DesiredEvidence x'1) (DesiredEvidence y'1) = DesiredEvidence (P'.mergeAppend x'1 y'1)
 
instance P'.Default DesiredEvidence where
  defaultValue = DesiredEvidence P'.defaultValue
 
instance P'.Wire DesiredEvidence where
  wireSize ft' self'@(DesiredEvidence x'1)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeRep 1 11 x'1)
  wirePut ft' self'@(DesiredEvidence x'1)
   = case ft' of
       10 -> put'Fields
       11 -> do
               P'.putSize (P'.wireSize 10 self')
               put'Fields
       _ -> P'.wirePutErr ft' self'
    where
        put'Fields
         = do
             P'.wirePutRep 10 11 x'1
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{evidenceDescriptor = P'.append (evidenceDescriptor old'Self) new'Field})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> DesiredEvidence) DesiredEvidence where
  getVal m' f' = f' m'
 
instance P'.GPB DesiredEvidence
 
instance P'.ReflectDescriptor DesiredEvidence where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Demo2.DesiredEvidence\", haskellPrefix = [], parentModule = [MName \"Demo2\"], baseName = MName \"DesiredEvidence\"}, descFilePath = [\"Demo2\",\"DesiredEvidence.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Demo2.DesiredEvidence.evidenceDescriptor\", haskellPrefix' = [], parentModule' = [MName \"Demo2\",MName \"DesiredEvidence\"], baseName' = FName \"evidenceDescriptor\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = True, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Demo2.EvidenceDescriptor\", haskellPrefix = [], parentModule = [MName \"Demo2\"], baseName = MName \"EvidenceDescriptor\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"