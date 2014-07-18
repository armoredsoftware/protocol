{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Demo2.ProtoResponse (ProtoResponse(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Demo2.EvidencePackage as Demo2 (EvidencePackage)
import qualified Demo2.QuotePackage as Demo2 (QuotePackage)
 
data ProtoResponse = ProtoResponse{evidencePackage :: !(P'.Maybe Demo2.EvidencePackage),
                                   quotePackage :: !(P'.Maybe Demo2.QuotePackage)}
                   deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable ProtoResponse where
  mergeAppend (ProtoResponse x'1 x'2) (ProtoResponse y'1 y'2) = ProtoResponse (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2)
 
instance P'.Default ProtoResponse where
  defaultValue = ProtoResponse P'.defaultValue P'.defaultValue
 
instance P'.Wire ProtoResponse where
  wireSize ft' self'@(ProtoResponse x'1 x'2)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 11 x'1 + P'.wireSizeOpt 1 11 x'2)
  wirePut ft' self'@(ProtoResponse x'1 x'2)
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
  wireGet ft'
   = case ft' of
       10 -> P'.getBareMessageWith update'Self
       11 -> P'.getMessageWith update'Self
       _ -> P'.wireGetErr ft'
    where
        update'Self wire'Tag old'Self
         = case wire'Tag of
             10 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{evidencePackage = P'.mergeAppend (evidencePackage old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             18 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{quotePackage = P'.mergeAppend (quotePackage old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> ProtoResponse) ProtoResponse where
  getVal m' f' = f' m'
 
instance P'.GPB ProtoResponse
 
instance P'.ReflectDescriptor ProtoResponse where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Demo2.ProtoResponse\", haskellPrefix = [], parentModule = [MName \"Demo2\"], baseName = MName \"ProtoResponse\"}, descFilePath = [\"Demo2\",\"ProtoResponse.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Demo2.ProtoResponse.evidencePackage\", haskellPrefix' = [], parentModule' = [MName \"Demo2\",MName \"ProtoResponse\"], baseName' = FName \"evidencePackage\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Demo2.EvidencePackage\", haskellPrefix = [], parentModule = [MName \"Demo2\"], baseName = MName \"EvidencePackage\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Demo2.ProtoResponse.quotePackage\", haskellPrefix' = [], parentModule' = [MName \"Demo2\",MName \"ProtoResponse\"], baseName' = FName \"quotePackage\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Demo2.QuotePackage\", haskellPrefix = [], parentModule = [MName \"Demo2\"], baseName = MName \"QuotePackage\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"