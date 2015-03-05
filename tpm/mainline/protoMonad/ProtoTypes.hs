{-# LANGUAGE TypeSynonymInstances #-}

module ProtoTypes where

import Data.ByteString.Lazy
import Data.Binary
import VChanUtil
import Codec.Crypto.RSA hiding (sign, verify)
import TPM.Types

--Abstract entity identifier.  The id assignments are LOCAL to the particular protocol being represented.
type EntityId = Int

type Nonce = Int

data TPM_DATA = 
  TdTPM_IDENTITY_CONTENTS  TPM_IDENTITY_CONTENTS 
  | TdTPM_KEY_HANDLE TPM_KEY_HANDLE 
              
              
              deriving (Show)
  
--Common data that is sent or received by an armored entity.
data ArmoredData =
  ANonce Nonce
  | AEntityInfo EntityInfo
  | ACipherText CipherText 
 -- | ATPM_DATA TPM_DATA 
  | ATPM_PCR_SELECTION TPM_PCR_SELECTION
  | ATPM_PCR_COMPOSITE TPM_PCR_COMPOSITE
  | ATPM_IDENTITY_CONTENTS TPM_IDENTITY_CONTENTS
  | ATPM_PUBKEY TPM_PUBKEY
  | ASignedData (SignedData ArmoredData)
  | ASignature Signature
  | AEvidenceDescriptor EvidenceDescriptor 
  | AEvidence Evidence deriving (Show)
--TODO:  Should the following "Command" items be message items(ArmoredData) that must be evaluated in the monad prior to sending?  For now, they are implemented as seperate explicit monadic function calls.
{-| GenNonce
  | Encrypt [ArmoredData]
  | Decrypt CipherText -} 

--Binary instance necessary for communication, encryption, and decryption(or a combination of these).
instance Binary ArmoredData where
  put (ANonce n) = do put (0::Word8)
                      put n
  --put (AEntityId id) = do put (1::Word8)
                      --    put id
  put (ACipherText ct) = do put(2::Word8)
                            put ct
  put (AEntityInfo einfo) = 
    do 
      put(3::Word8)
      put einfo
  put (AEvidenceDescriptor e) = 
    do 
      put (4::Word8)
      put e
  put(ATPM_PCR_SELECTION s) = 
    do
      put(5::Word8)
      put s
  put(ATPM_PUBKEY p) = 
    do
      put(6::Word8)
      put p
  put(ASignedData s) = 
    do
      put(7::Word8)
      put s
  put(ASignature a) = 
    do
      put(8::Word8)
      put a
  put(ATPM_PCR_COMPOSITE p) = 
    do
      put(9::Word8)
      put p
  put(ATPM_IDENTITY_CONTENTS i) = 
    do
      put(10::Word8)
      put i 
  put(AEvidence e) = 
    do
      put(11::Word8)
      put e

  get = do t <- get :: Get Word8
           case t of
             0 -> do n <- get
                     return $ ANonce n
            -- 1 -> do id <- get
                 --    return $ AEntityId id
             2 -> do ct <- get
                     return $ ACipherText ct
             3 -> do einfo <- get
                     return $ AEntityInfo einfo
             
             4 -> do e <- get
                     return $ AEvidenceDescriptor e
             5 -> do s <- get
                     return $ ATPM_PCR_SELECTION s
             6 -> do s <- get
                     return $ ATPM_PUBKEY s
             7 -> do s <- get
                     return $ ASignedData s
             8 -> do s <- get
                     return $ ASignature s
             9 -> do s <- get
                     return $ ATPM_PCR_COMPOSITE s
             10 -> do s <- get
                      return $ ATPM_IDENTITY_CONTENTS s 
             11 -> do s <- get
                      return $ AEvidence s
             

type Message = [ArmoredData]
  

--This shoud contain the concrete info that is necessary to communicate with an entity
data EntityInfo = EntityInfo {
  entityName :: String,
  entityIp :: Int,
  vChan :: LibXenVChan
} deriving (Eq, Show)

instance Binary EntityInfo where
  put (EntityInfo name ip vchan) = 
    do 
      put name
      put ip

  get = do name <- get
           ip <- get
           let vchan = undefined --vchan <- get  --IS THIS OK??
           return $ EntityInfo name ip vchan
  
  
type PrivateKey = Codec.Crypto.RSA.PrivateKey --ByteString;
type PublicKey = Codec.Crypto.RSA.PublicKey --ByteString;

type SymmKey = TPM_SYMMETRIC_KEY 
--Encrypted text
type CipherText = ByteString;

type EvidenceDescriptor = [Int]
type Evidence = [Int]

type Signature = ByteString;
data SignedData a = SignedData {
  dat :: a,
  sig :: Signature
} deriving (Show)

instance (Binary a) => Binary (SignedData a) where
  put (SignedData a b) = 
    do 
      put a
      put b
      
  get = do a <- get
           b <- get
           return $ SignedData a b 

type AikContents = SignedData TPM_IDENTITY_CONTENTS





