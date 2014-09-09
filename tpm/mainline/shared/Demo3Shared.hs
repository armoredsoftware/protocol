module Demo3Shared where

import TPM
import Data.Binary
import Data.ByteString.Lazy(ByteString, empty, append)
{-
-- utility libraries
import Data.Binary
import Data.ByteString (ByteString, pack, append, empty)
import qualified Data.ByteString as B
import System.IO
import System.IO.Unsafe (unsafePerformIO)

-- crypto libraries
import Crypto.PubKey.RSA
import Crypto.Hash.MD5(hash)
-}

tpm :: TPMSocket
tpm = tpm_socket "/var/run/tpm/tpmd_socket:0"

appId :: Int
appId = 20

attId :: Int
attId = 19

meaId :: Int
meaId = 3

ownerPass :: String
ownerPass = "adam"

srkPass :: String
srkPass = ""




data Shared = Appraisal Request
              | Attestation Response
              | Result Bool



instance Show Shared where
    show (Appraisal app) = "Appraisal: " ++ show app
    show (Attestation att) = "Attestation: " ++ show att
    show (Result True) = "Appraisal succeeded."
    show (Result False) = "Appraisal failed."
    
instance Binary Shared where
  put (Appraisal app)             = do put (0::Word8)
                                       put app
  put (Attestation att)           = do put (1::Word8)
                                       put att
  put (Result res)                = do put(2::Word8)
                                       put res

  get = do t<- get :: Get Word8
           case t of
             0 -> do app <- get
                     return (Appraisal app)
             1 -> do att <- get
                     return (Attestation att)
             2 -> do res <- get
                     return (Result res)
                     




-- Primitive types
--type PCR = Word8 --TPM_PCRVALUE
--type Nonce = ByteString --TPM_NONCE
type Signature = ByteString
--type TPMRequest = Word8 --TPM_PCR_SELECTION
--type Quote = (([PCR], Nonce), Signature)--simulates TPM
type Quote = (TPM_PCR_COMPOSITE, Signature)

--Request
type Request = (DesiredEvidence, TPM_PCR_SELECTION, TPM_NONCE)
type DesiredEvidence = [EvidenceDescriptor]
data EvidenceDescriptor = D0 | D1 | D2 deriving(Eq, Ord) --for now

instance Binary EvidenceDescriptor where
  put D0 = put (0::Word8)
  put D1 = put (1::Word8)
  put D2 = put (2::Word8)
           
  get = do t<- get :: Get Word8
           case t of
               0 -> return D0
               1 -> return D1
               2 -> return D2
                    

instance Show EvidenceDescriptor where
  show D0 = "Desired: Measurement #0"
  show D1 = "Desired: Measurement #1"
  show D2 = "Desired: Measurement #2"
   

--Response
type Response = (EvidencePackage, Quote)
type EvidencePackage = (Evidence, TPM_NONCE, Signature)
type Evidence = [EvidencePiece]


 
data EvidencePiece = M0 M0Rep 
                   | M1 M1Rep
                   | M2 M2Rep deriving (Eq, Ord, Show)

type M0Rep = ByteString
type M1Rep = ByteString
type M2Rep = ByteString

instance Binary EvidencePiece where
         put (M0 req) = do put (0::Word8);
                             put req;
         put(M1 quote) =  do put (1::Word8);
                               put quote;
         put(M2 res)= do put(2::Word8);
                           put res;
                                            
         get = do t<- get :: Get Word8
                  case t of
                    0 -> do req <- get
                            return (M0 req)
                    1 -> do quote <- get
                            return (M1 quote)
                    2 -> do res <- get
                            return (M2 res)
                         
         
ePack :: Evidence -> TPM_NONCE -> ByteString
ePack e (TPM_NONCE n) = ePack' e `append` n

--This is where we will need to convert measurement type to ByteString
-- if it is something else.  see comment below
ePack' :: Evidence -> ByteString
ePack'  = foldr f empty 
  where f (M0 x) y = x `append` y -- (i.e. (toByteString x) `append` y )
        f (M1 x) y = x `append` y
        f (M2 x) y = x `append` y          
         
         
--type Hash = ByteString
--type QuotePackage = (Quote, Hash, Signature)



{-

ePack :: Evidence -> Nonce -> ByteString
ePack e n = ePack' e `append` n

--This is where we will need to convert measurement type to ByteString
-- if it is something else.  see comment below
ePack' :: Evidence -> ByteString
ePack'  = foldr f empty 
  where f (M0 x) y = x `append` y -- (i.e. (toByteString x) `append` y )
        f (M1 x) y = x `append` y
        f (M2 x) y = x `append` y

qPack :: Quote -> Hash -> ByteString
qPack q@((pcrsIn, nonce), sig) hash = 
  tPack (pcrsIn, nonce) `append` sig `append` hash
  
tPack :: ([PCR], Nonce) -> ByteString
tPack (pcrs, nonce) = pack pcrs `append` nonce


doHash :: ByteString -> ByteString
doHash = hash

-}