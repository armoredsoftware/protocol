{-# LANGUAGE BangPatterns, DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses #-}
module Demo2.EvidencePiece.Constructor (Constructor(..)) where
import Prelude ((+), (/), (.))
import qualified Prelude as Prelude'
import qualified Data.Typeable as Prelude'
import qualified Data.Data as Prelude'
import qualified Text.ProtocolBuffers.Header as P'
 
data Constructor = M0
                 | M1
                 | M2
                 | M3
                 deriving (Prelude'.Read, Prelude'.Show, Prelude'.Eq, Prelude'.Ord, Prelude'.Typeable, Prelude'.Data)
 
instance P'.Mergeable Constructor
 
instance Prelude'.Bounded Constructor where
  minBound = M0
  maxBound = M3
 
instance P'.Default Constructor where
  defaultValue = M0
 
toMaybe'Enum :: Prelude'.Int -> P'.Maybe Constructor
toMaybe'Enum 0 = Prelude'.Just M0
toMaybe'Enum 1 = Prelude'.Just M1
toMaybe'Enum 2 = Prelude'.Just M2
toMaybe'Enum 3 = Prelude'.Just M3
toMaybe'Enum _ = Prelude'.Nothing
 
instance Prelude'.Enum Constructor where
  fromEnum M0 = 0
  fromEnum M1 = 1
  fromEnum M2 = 2
  fromEnum M3 = 3
  toEnum
   = P'.fromMaybe (Prelude'.error "hprotoc generated code: toEnum failure for type Demo2.EvidencePiece.Constructor") . toMaybe'Enum
  succ M0 = M1
  succ M1 = M2
  succ M2 = M3
  succ _ = Prelude'.error "hprotoc generated code: succ failure for type Demo2.EvidencePiece.Constructor"
  pred M1 = M0
  pred M2 = M1
  pred M3 = M2
  pred _ = Prelude'.error "hprotoc generated code: pred failure for type Demo2.EvidencePiece.Constructor"
 
instance P'.Wire Constructor where
  wireSize ft' enum = P'.wireSize ft' (Prelude'.fromEnum enum)
  wirePut ft' enum = P'.wirePut ft' (Prelude'.fromEnum enum)
  wireGet 14 = P'.wireGetEnum toMaybe'Enum
  wireGet ft' = P'.wireGetErr ft'
  wireGetPacked 14 = P'.wireGetPackedEnum toMaybe'Enum
  wireGetPacked ft' = P'.wireGetErr ft'
 
instance P'.GPB Constructor
 
instance P'.MessageAPI msg' (msg' -> Constructor) Constructor where
  getVal m' f' = f' m'
 
instance P'.ReflectEnum Constructor where
  reflectEnum = [(0, "M0", M0), (1, "M1", M1), (2, "M2", M2), (3, "M3", M3)]
  reflectEnumInfo _
   = P'.EnumInfo (P'.makePNF (P'.pack ".Demo2.EvidencePiece.Constructor") [] ["Demo2", "EvidencePiece"] "Constructor")
      ["Demo2", "EvidencePiece", "Constructor.hs"]
      [(0, "M0"), (1, "M1"), (2, "M2"), (3, "M3")]