{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module Demo3Shared where

import TPM
import Data.Binary
import Data.ByteString.Lazy(ByteString, empty, append, pack, toStrict, fromStrict)
import Codec.Crypto.RSA hiding (sign, verify)
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



sign :: Binary a => PrivateKey -> a -> ByteString
sign priKey a = rsassa_pkcs1_v1_5_sign ha_SHA1 priKey (encode a)

verify :: Binary a => PublicKey -> a -> ByteString -> Bool
verify pubKey a sig = rsassa_pkcs1_v1_5_verify ha_SHA1 pubKey (encode a) 
                                                                          sig
  
                      
signPack :: Binary a => PrivateKey -> a -> Signed a
signPack priKey x = Signed x sig
  where sig = sign priKey x



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

--Abstract datatype for signed payloads
data Signed a = Signed {
  x :: a, 
  y :: Signature
  } deriving Show

--Request
data Request = Request {
  desiredE :: DesiredEvidence,
  pcrSelect :: TPM_PCR_SELECTION,
  nonce :: TPM_NONCE
  } deriving (Show)
             
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


type Quote = Signed TPM_PCR_COMPOSITE

--Response
data Response = Response {
  evPack :: EvidencePackage, 
  caCert :: CACertificate,
  quote :: Quote
  } deriving (Show)
             
data EvidencePackage = EvidencePackage {
  evList :: Evidence, 
  eNonce :: TPM_NONCE,
  eSig :: Signature
  } deriving (Show)
             

    
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
         
         
      
type MakeIdResult = Signed TPM_IDENTITY_CONTENTS

type PlatformID = Int  
type SessionKey = ByteString --Is this helpful?
type Encrypted = ByteString --Is this helpful?

data CARequest = CARequest {
  pId :: PlatformID, 
  mkIdResult :: MakeIdResult
  } deriving (Show)
             
data CAResponse = CAResponse {
  encCACert :: Encrypted, 
  encActIdInput :: Encrypted
  } deriving (Show)

type CACertificate = Signed TPM_PUBKEY   

data ActivateIdRequest = ActivateIdRequest {
  sessKey :: SessionKey, 
  aikDigest :: TPM_DIGEST
  } deriving (Show)
                     

{-                  
--type Hash = ByteString
--type QuotePackage = (Quote, Hash, Signature)

doHash :: ByteString -> ByteString
doHash = hash

-}


--Boilerplate Binary instances(remove if there is an easier way to generate)--
instance Binary Quote where
  put(Signed a b) = do
    put a
    put b
  get = do
    a <- get
    b <- get
    return (Signed a b)
    
instance Binary MakeIdResult where
  put(Signed a b) = do
    put a
    put b
  get = do
    a <- get
    b <- get
    return (Signed a b)
    
instance Binary CACertificate where
  put(Signed a b) = do
    put a
    put b
  get = do
    a <- get
    b <- get
    return (Signed a b)
    
    
instance Binary Request where
  put(Request a b c) = do
    put a
    put b
    put c
  get = do
    a <- get
    b <- get
    c <- get
    return $ Request a b c
    
instance Binary Response where
  put(Response a b c) = do
    put a
    put b
    put c
  get = do
    a <- get
    b <- get
    c <- get
    return $ Response a b c
    
instance Binary EvidencePackage where
  put(EvidencePackage a b c) = do
    put a
    put b
    put c
  get = do
    a <- get
    b <- get
    c <- get
    return $ EvidencePackage a b c
    
             
instance Binary  CARequest where
  put(CARequest a b ) = do
    put a
    put b
  get = do
    a <- get
    b <- get
    return $ CARequest a b 
             
instance Binary  CAResponse where
  put(CAResponse a b ) = do
    put a
    put b
  get = do
    a <- get
    b <- get
    return $ CAResponse a b 
    
instance Binary ActivateIdRequest where
  put(ActivateIdRequest a b ) = do
    put a
    put b
  get = do
    a <- get
    b <- get
    return $ ActivateIdRequest a b 