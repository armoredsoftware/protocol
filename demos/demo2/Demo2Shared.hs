{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}
module Demo2Shared where

-- utility libraries
import Data.Binary
import qualified Data.ByteString as B (ByteString, pack, append, empty) 


import qualified Data.ByteString as B
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import GHC.Generics as G

-- crypto libraries
import Crypto.PubKey.RSA
import Crypto.Hash.MD5(hash)


                            
-- Primitive types
type PCR = Word8
type Nonce = [Word8]
type Signature =[Word8]
type TPMRequest = Word8 -- Request = (Mask, Nonce)
data Quote = Quote { pcrList_Quote ::[PCR],
                     nonce_Quote ::  Nonce,
                     signature_Quote :: Signature} deriving (Show) --simulates TPM 

--Request
data Request = Request { desiredEvidence_Request ::DesiredEvidence,
                         tpm_Request :: TPMRequest,
                         nonce_Request :: Nonce} deriving (Show)
               
data DesiredEvidence = DesiredEvidence {evidenceDescriptorList_DesiredEvidence :: [EvidenceDescriptor]} deriving (Show)
data EvidenceDescriptor = D0 |
                          D1 |
                          D2 deriving(Eq, Ord, Generic) --for now
            

instance Show EvidenceDescriptor where
  show D0 = "Measurement #0"
  show D1 = "Measurement #1"
  show D2 = "Measurement #2"
   

--Response
data Response = Response {evidencePackage_Response :: EvidencePackage,
                          quotePackage_Response :: QuotePackage} deriving (Show)
                
data EvidencePackage = EvidencePackage {evidence_EvidencePackage :: Evidence,
                                        nonce_EvidencePackage :: Nonce,
                                        signature_EvidencePackage :: Signature} deriving (Show)
data Evidence = Evidence {evidencePieceList_Evidence :: [EvidencePiece]} deriving (Show)
 
data EvidencePiece =  M0 {m0Rep_EvidencePiece ::  M0Rep} 
                   | M1 {m1Rep_EvidencePiece :: M1Rep}
                   | M2 {m2Rep_EvidencePiece :: M2Rep} deriving (Eq, Ord, Show, Generic)
                         
type Hash = [Word8]
data QuotePackage = QuotePackage { quote_QuotePackage :: Quote,
                                   hash_QuotePackage :: Hash,
                                   signature_QuotePackage :: Signature} deriving (Show)

--changed to work well with JSON from ByteStrings
type M0Rep = [Word8]
type M1Rep = [Word8]
type M2Rep = [Word8] 

doHash :: B.ByteString -> B.ByteString
doHash = hash



