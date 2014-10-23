{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module Demo3Shared where

import TPM
import VChanUtil
import Data.Binary
import Data.ByteString.Lazy(ByteString, empty, append, pack, toStrict, fromStrict)
import qualified Data.ByteString as B (ByteString)
import Codec.Crypto.RSA hiding (sign, verify)
import System.Random
import Crypto.Cipher.AES


tpm :: TPMSocket
tpm = tpm_socket "/var/run/tpm/tpmd_socket:0"

appId :: Int
appId = 20

appName :: String
appName = "Appraiser"

attId :: Int
attId = 19

attName :: String
attName = "Attester"

meaId :: Int
meaId = 7

meaName :: String
meaName = "Measurer"

caId :: Int
caId = 21

caName :: String
caName = "CA"

ownerPass :: String
ownerPass = "adam"

srkPass :: String
srkPass = ""

exportEKFileName = "attEKPubKey.txt"

exportCAPubFileName = "appCAPublicKey.txt"


type PubKey = Codec.Crypto.RSA.PublicKey
type PriKey = Codec.Crypto.RSA.PrivateKey

type SymKey = B.ByteString

generateCAKeyPair :: (PubKey, PriKey)
generateCAKeyPair = let gen = mkStdGen 3
                        (pub, pri, _) = generateKeyPair gen 2048 in (pub, pri)

encrypt :: Binary a => SymKey -> a -> ByteString
encrypt key m = (fromStrict encryptedStrictM)

 where encodedM = encode m
       strictM = toStrict encodedM
       --strictKey = toStrict lazyKey
       aes = initAES key
       ctr = key
       encryptedStrictM = encryptCTR aes ctr strictM
       --lazyEncryptedStrictM = fromStrict encryptedStrictM

decrypt :: SymKey -> ByteString -> ByteString
decrypt key encryptedM = (fromStrict strictDecryptedM)

 where strictEncryptedM = toStrict encryptedM
       aes = initAES key
       ctr = key
       strictDecryptedM = decryptCTR aes ctr strictEncryptedM

sign :: (Binary a{-, Signable a-}) => PriKey -> a -> ByteString
sign priKey a = rsassa_pkcs1_v1_5_sign ha_SHA1 priKey ({-toBits-} encode a)

verify :: (Binary a{-, Signable a-})=> PubKey -> Signed a -> Bool
verify pubKey signed = rsassa_pkcs1_v1_5_verify ha_SHA1 
                                                                          pubKey 
                                                                          ({-toBits-}encode $ dat signed) 
                                                                          (sig signed)
  
                      
signPack :: (Binary a{-, Signable a-}) => PriKey -> a -> Signed a
signPack priKey x = Signed x sig
  where sig = sign priKey x


sendM :: (Binary a, Show a) => String -> LibXenVChan ->  a -> IO ()
sendM descrip chan m = do
  putStrLn $ descrip ++ "Sending: " ++ show m ++ "\n"
  send chan $ m
  return () 


sendR :: (Binary a, Show a) => PlatformID -> String -> a -> IO LibXenVChan
sendR dest descrip req = do
    chan <- client_init dest
    putStrLn $ descrip ++ "Sending: " ++ show req ++ "\n"
    send chan $ req
    return chan

receiveM :: (Binary a, Show a) => String -> LibXenVChan -> IO a
receiveM descrip chan = do
  ctrlWait chan
  res <- receive chan
  putStrLn $ descrip ++ "Received: " ++ show res ++ "\n"
  return res


process :: (Binary a, Show a, Binary b, Show b) => (LibXenVChan -> IO a) 
                -> (LibXenVChan -> b -> IO ()) -> (a -> IO b) -> LibXenVChan -> IO ()
process recA sendB mk chan = do
  --ctrlWait chan
  req <- recA chan
  resp <- mk req
  sendB chan resp
  return ()


{-
class Signable a where
  toBits :: Binary a => a -> ByteString
  toBits = encode
-}

--instance Signable TPM_QUOTE_INFO



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
  dat :: a, 
  sig :: Signature
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


--type Quote = (TPM_PCR_COMPOSITE, Signature)
--type Quote = Signed TPM_PCR_COMPOSITE

data Quote = Quote {
  pcrComposite :: TPM_PCR_COMPOSITE,
  qSig :: Signature
  } deriving (Show)

--instance Signable TPM_PUBKEY

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
                         
       
ePack :: Evidence -> TPM_NONCE -> TPM_PUBKEY -> ByteString
ePack e (TPM_NONCE n) pubKey = ePack' e `append` (encode pubKey) 
                                                                    `append` n

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
  put(Quote a b) = do
    put a
    put b
  get = do
    a <- get
    b <- get
    return (Quote a b)

    
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