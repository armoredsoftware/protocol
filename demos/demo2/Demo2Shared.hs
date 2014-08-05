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

data Shared = Appraisal Request
              | Attestation Response
              | Result Bool



instance Show Shared where
    show (Appraisal app) = "Appraisal: " ++ (show app)
    show (Attestation att) = "Attestation: " ++ (show att)
    show (Result True) = "Appraisal succeeded."
    show (Result False) = "Appraisal failed."
    
instance Binary Shared where
  put (Appraisal req)              = do put (0::Word8)
                                        put req
  put(Attestation quote)           = do put (1::Word8)
  		  		     	put quote
  put(Result res)                  = do put(2::Word8)
                                        put res

  get = do t<- get :: Get Word8
           case t of
             0 -> do req <- get
                     return (Appraisal req)
             1 -> do quote <- get
                     return (Attestation quote)
             2 -> do res <- get
                     return (Result res)
                     
instance Binary EvidencePiece where
         put (Demo2Shared.M0 req) = do put (0::Word8);
                                       put req;
         put(Demo2Shared.M1 quote) =  do put (1::Word8);
                                         put quote;
         put(Demo2Shared.M2 res)= do put(2::Word8);
                                     put res;
                                            
         get = do t<- get :: Get Word8
                  case t of
                    0 -> do req <- get
                            return (Demo2Shared.M0 req)
                    1 -> do quote <- get
                            return (Demo2Shared.M1 quote)
                    2 -> do res <- get
                            return (Demo2Shared.M2 res)



-- Primitive types
type PCR = Word8
type Nonce = [Word8]
type Signature =[Word8]
type TPMRequest = Word8 -- Request = (Mask, Nonce)
data Quote = Quote { pcrList ::[PCR],
                     nonceQuote ::  Nonce,
                     signatureQuote :: Signature}--simulates TPM 

--Request
data Request = Request { desiredEvidence ::DesiredEvidence,
                         tpmRequest :: TPMRequest,
                         nonceRequest :: Nonce}
               
data DesiredEvidence = DesiredEvidence {evidenceDescriptorList :: [EvidenceDescriptor]}
data EvidenceDescriptor = D0 |
                          D1 |
                          D2 deriving(Eq, Ord, Generic) --for now

instance Binary EvidenceDescriptor where
  put D0 = do put (0::Word8)
  put D1 = do put (1::Word8)
  put D2 = do put (2::Word8)
           
  get = do t<- get :: Get Word8
           case t of
               0 -> return D0
               1 -> return D1
               2 -> return D2
                    

instance Show EvidenceDescriptor where
  show D0 = "Measurement #0"
  show D1 = "Measurement #1"
  show D2 = "Measurement #2"
   

--Response
data Response = Response {evidencePackage :: EvidencePackage,
                          quotePackage :: QuotePackage}
                
data EvidencePackage = EvidencePackage {evidence :: Evidence,
                                        nonceEvidencePackage :: Nonce,
                                        signatureEvidencePackage :: Signature}
data Evidence = Evidence {evidencePieceList :: [EvidencePiece]}
 
data EvidencePiece =  M0 {m0Rep ::  M0Rep} 
                   | M1 {m1Rep :: M1Rep}
                   | M2 {m2Rep :: M2Rep} deriving (Eq, Ord, Show, Generic)
                         
type Hash = [Word8]
data QuotePackage = QuotePackage { quoteQuotePackage :: Quote,
                                   hashQuotePackage :: Hash,
                                   signatureQuotePackage :: Signature}

--changed to work well with JSON from ByteStrings
type M0Rep = [Word8]
type M1Rep = [Word8]
type M2Rep = [Word8]


ePack :: Evidence -> Nonce -> B.ByteString
ePack e n = (ePack' e) `B.append` (B.pack n) --pik

--This is where we will need to convert measurement type to ByteString
-- if it is something else.  see comment below
ePack' :: Evidence -> B.ByteString
ePack' es = foldr f B.empty (evidencePieceList es)
  where f (Demo2Shared.M0 x) y =(B.pack $ m0Rep  x) `B.append` (B.pack y) -- (i.e. (toByteString x) `append` y )
        f (Demo2Shared.M1 x) y = (B.pack $ m1Rep x) `B.append` (B.pack y)
        f (Demo2Shared.M2 x) y =(B.pack $ m2Rep x) `B.append` (B.pack y)

qPack :: Quote -> Hash -> B.ByteString
qPack q hash = 
  (tPack ((pcrList q), (nonceQuote q))) `B.append` (signatureQuote q) `B.append` (B.pack hash)
  
tPack :: ([PCR], Nonce) -> B.ByteString
tPack (pcrs, nonce) = B.pack pcrs `B.append` (B.pack nonce)


doHash :: B.ByteString -> B.ByteString
doHash = hash



