{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module Demo3Shared where

import TPM
import Data.Binary
import Data.ByteString.Lazy(ByteString, empty, append, pack, toStrict, fromStrict)
import Codec.Crypto.RSA
import System.Random
import Crypto.Cipher.AES


tpm :: TPMSocket
tpm = tpm_socket "/var/run/tpm/tpmd_socket:0"

appId :: Int
appId = 20

attId :: Int
attId = 19

meaId :: Int
meaId = 7

caId :: Int
caId = 21

ownerPass :: String
ownerPass = "adam"

srkPass :: String
srkPass = ""

exportEKFileName = "attEKPubKey.txt"

exportCAPubFileName = "appCAPublicKey.txt"


generateCAKeyPair :: (PublicKey, PrivateKey)
generateCAKeyPair = let gen = mkStdGen 3
                        (pub, pri, _) = generateKeyPair gen 2048 in (pub, pri)



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
type Signature = ByteString
type Signed a = (a, Signature)

{-
instance Binary (Signed a) where
  put = undefined
  get = undefined
-}
  
type Quote = Signed TPM_PCR_COMPOSITE

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
   

--type PubKeyRequest = Bool
--type PubKeyResponse = TPM_PUBKEY

--Response
type Response = (EvidencePackage, CACertificate, Quote)
type EvidencePackage = (Evidence, TPM_NONCE, Signature) --Remove sig now?
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

ePackSilly :: Evidence -> TPM_NONCE -> ByteString
ePackSilly e (TPM_NONCE n) = n `append` ePack' e   

--This is where we will need to convert measurement type to ByteString
-- if it is something else.  see comment below
ePack' :: Evidence -> ByteString
ePack'  = foldr f empty 
  where f (M0 x) y = x `append` y -- (i.e. (toByteString x) `append` y )
        f (M1 x) y = x `append` y
        f (M2 x) y = x `append` y          
         
         
      
type MakeIdResult = Signed TPM_IDENTITY_CONTENTS --(TPM_IDENTITY_CONTENTS, Signature)  

type PlatformID = Int  
type SessionKey = ByteString --Is this helpful?
type Encrypted = ByteString --Is this helpful?
type CARequest = (PlatformID, MakeIdResult)    
type CAResponse = (Encrypted, Encrypted) --Make type Encrypted = ByteString?
type CACertificate = Signed TPM_PUBKEY --(TPM_PUBKEY, Signature)      
type ActivateIdRequest = (SessionKey, TPM_DIGEST)                  
                     





--type DecryptedCAResponse = (CACertificate, ActivateIdRequest)
        
        
{-                  
--type Hash = ByteString
--type QuotePackage = (Quote, Hash, Signature)

doHash :: ByteString -> ByteString
doHash = hash

-}
