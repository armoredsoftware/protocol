{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Demo2.QuotePackage (QuotePackage(..)) where
import Prelude ((+), (/))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
import qualified Demo2.Hash as Demo2 (Hash)
import qualified Demo2.Quote as Demo2 (Quote)
import qualified Demo2.Signature as Demo2 (Signature)
 
data QuotePackage = QuotePackage{quote :: !(P'.Maybe Demo2.Quote), hash :: !(P'.Maybe Demo2.Hash),
                                 signature :: !(P'.Maybe Demo2.Signature)}
                  deriving (Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable QuotePackage where
  mergeAppend (QuotePackage x'1 x'2 x'3) (QuotePackage y'1 y'2 y'3)
   = QuotePackage (P'.mergeAppend x'1 y'1) (P'.mergeAppend x'2 y'2) (P'.mergeAppend x'3 y'3)
 
instance P'.Default QuotePackage where
  defaultValue = QuotePackage P'.defaultValue P'.defaultValue P'.defaultValue
 
instance P'.Wire QuotePackage where
  wireSize ft' self'@(QuotePackage x'1 x'2 x'3)
   = case ft' of
       10 -> calc'Size
       11 -> P'.prependMessageSize calc'Size
       _ -> P'.wireSizeErr ft' self'
    where
        calc'Size = (P'.wireSizeOpt 1 11 x'1 + P'.wireSizeOpt 1 11 x'2 + P'.wireSizeOpt 1 11 x'3)
  wirePut ft' self'@(QuotePackage x'1 x'2 x'3)
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
             10 -> Prelude'.fmap (\ !new'Field -> old'Self{quote = P'.mergeAppend (quote old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             18 -> Prelude'.fmap (\ !new'Field -> old'Self{hash = P'.mergeAppend (hash old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             26 -> Prelude'.fmap
                    (\ !new'Field -> old'Self{signature = P'.mergeAppend (signature old'Self) (Prelude'.Just new'Field)})
                    (P'.wireGet 11)
             _ -> let (field'Number, wire'Type) = P'.splitWireTag wire'Tag in P'.unknown field'Number wire'Type old'Self
 
instance P'.MessageAPI msg' (msg' -> QuotePackage) QuotePackage where
  getVal m' f' = f' m'
 
instance P'.GPB QuotePackage
 
instance P'.ReflectDescriptor QuotePackage where
  getMessageInfo _ = P'.GetMessageInfo (P'.fromDistinctAscList []) (P'.fromDistinctAscList [10, 18, 26])
  reflectDescriptorInfo _
   = Prelude'.read
      "DescriptorInfo {descName = ProtoName {protobufName = FIName \".Demo2.QuotePackage\", haskellPrefix = [], parentModule = [MName \"Demo2\"], baseName = MName \"QuotePackage\"}, descFilePath = [\"Demo2\",\"QuotePackage.hs\"], isGroup = False, fields = fromList [FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Demo2.QuotePackage.quote\", haskellPrefix' = [], parentModule' = [MName \"Demo2\",MName \"QuotePackage\"], baseName' = FName \"quote\"}, fieldNumber = FieldId {getFieldId = 1}, wireTag = WireTag {getWireTag = 10}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Demo2.Quote\", haskellPrefix = [], parentModule = [MName \"Demo2\"], baseName = MName \"Quote\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Demo2.QuotePackage.hash\", haskellPrefix' = [], parentModule' = [MName \"Demo2\",MName \"QuotePackage\"], baseName' = FName \"hash\"}, fieldNumber = FieldId {getFieldId = 2}, wireTag = WireTag {getWireTag = 18}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Demo2.Hash\", haskellPrefix = [], parentModule = [MName \"Demo2\"], baseName = MName \"Hash\"}), hsRawDefault = Nothing, hsDefault = Nothing},FieldInfo {fieldName = ProtoFName {protobufName' = FIName \".Demo2.QuotePackage.signature\", haskellPrefix' = [], parentModule' = [MName \"Demo2\",MName \"QuotePackage\"], baseName' = FName \"signature\"}, fieldNumber = FieldId {getFieldId = 3}, wireTag = WireTag {getWireTag = 26}, packedTag = Nothing, wireTagLength = 1, isPacked = False, isRequired = False, canRepeat = False, mightPack = False, typeCode = FieldType {getFieldType = 11}, typeName = Just (ProtoName {protobufName = FIName \".Demo2.Signature\", haskellPrefix = [], parentModule = [MName \"Demo2\"], baseName = MName \"Signature\"}), hsRawDefault = Nothing, hsDefault = Nothing}], keys = fromList [], extRanges = [], knownKeys = fromList [], storeUnknown = False, lazyFields = False}"