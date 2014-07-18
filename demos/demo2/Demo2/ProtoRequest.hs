{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Demo2.ProtoRequest (ProtoRequest(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Demo2.DesiredEvidence as Demo2 (DesiredEvidence)
import qualified Demo2.Nonce as Demo2 (Nonce)
import qualified Demo2.TPMRequest as Demo2 (TPMRequest)
 
data ProtoRequest = ProtoRequest{desiredEvidence :: !(P'.Maybe Demo2.DesiredEvidence), tpmRequest :: !(P'.Maybe Demo2.TPMRequest),
                                 nonce :: !(P'.Maybe Demo2.Nonce)}
                  deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable ProtoRequest where
  mergeAppend (ProtoRequest x'1 x'2 x'3) (ProtoRequest y'1 y'2 y'3)
   = ProtoRequest (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default ProtoRequest where
  defaultValue = ProtoRequest P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire ProtoRequest where
  wireSize ft' self'@(ProtoRequest x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 11 x'1 + P'.wireSizeOpt 1 11 x'2 + P'.wireSizeOpt 1 11 x'3)
  wirePut ft' self'@(ProtoRequest x'1 x'2 x'3)
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
             10 -> Prelude'.fmap
                    (\ !new'Field ->
                      old'Self{desiredEvidence = P'.mergeAppend (desiredEvidence old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             18 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{tpmRequest = P'.mergeAppend (tpmRequest old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             26 -> Prelude'.fmap (\ !new'Field -> old'Self{nonce = P'.mergeAppend (nonce old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> ProtoRequest) ProtoRequest where
  getVal m' f' = f' m'
 
instance P'.GPB ProtoRequest
 
instance P'.ReflectDescriptor ProtoRequest where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Demo2.ProtoRequest\", haskellPrefix = [], parentModule = [MName \"Demo2\"], baseName = MName \"ProtoRequest\"}, descFilePath = [\"Demo2\",\"ProtoRequest.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Demo2.ProtoRequest.desiredEvidence\", haskellPrefix' = [], parentModule' = [MName \"Demo2\",MName \"ProtoRequest\"], baseName' = FName \"desiredEvidence\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Demo2.DesiredEvidence\", haskellPrefix = [], parentModule = [MName \"Demo2\"], baseName = MName \"DesiredEvidence\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Demo2.ProtoRequest.tpmRequest\", haskellPrefix' = [], parentModule' = [MName \"Demo2\",MName \"ProtoRequest\"], baseName' = FName \"tpmRequest\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Demo2.TPMRequest\", haskellPrefix = [], parentModule = [MName \"Demo2\"], baseName = MName \"TPMRequest\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Demo2.ProtoRequest.nonce\", haskellPrefix' = [], parentModule' = [MName \"Demo2\",MName \"ProtoRequest\"], baseName' = FName \"nonce\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Demo2.Nonce\", haskellPrefix = [], parentModule = [MName \"Demo2\"], baseName = MName \"Nonce\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"