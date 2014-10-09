{-# LANGUAGE ScopedTypeVariables #-}

module PrivacyCA where

import TPM
import Demo3Shared
import VChanUtil

import Data.Binary
import Data.Bits
import Data.ByteString.Lazy (ByteString, cons, empty, pack)
import Codec.Crypto.RSA
import System.Random
import Crypto.Cipher.AES
--import Codec.Crypto.AES
--withOpenSSL

import System.IO
{-
main :: IO ()
main = do putStrLn "main of Measurer"
-}


receiveCARequest :: LibXenVChan -> IO CARequest
receiveCARequest chan = do
  ctrlWait chan
  req :: CARequest <- receive chan
  return req
  
sendCAResponse :: LibXenVChan -> CAResponse -> IO ()
sendCAResponse chan resp = do
  putStrLn $ "CA Sending: " ++ show resp ++ "\n"
  send chan resp
  return () 


mkCAResponse :: CARequest -> IO CAResponse
mkCAResponse (id, (idContents, idSig)) = do
  ekPubKey <- readPubEK
  let iPubKey = identityPubKey idContents
      iDigest = tpm_digest $ encode iPubKey
      asymContents = contents iDigest
      blob = encode asymContents
      encBlob =  tpm_rsa_pubencrypt ekPubKey blob
      
      caPriKey = undefined
      signedAIK = rsassa_pkcs1_v1_5_sign ha_SHA1 caPriKey (encode iPubKey)
      --encryptedSignedAIK = crypt' CTR symKey symKey Encrypt signedAIK  
      --TODO:  above line will compile if I get AES dependency the same as on my mac
  return (empty, empty)
 where 
   x:: Word8 
   x = 1
   key = (Data.ByteString.Lazy.pack $ replicate 16 x) 
   symKey = 
     TPM_SYMMETRIC_KEY 
     (tpm_alg_aes128) 
     (tpm_es_sym_ctr) 
     key
     
   contents dig = TPM_ASYM_CA_CONTENTS symKey dig


generateCAKeyPair :: (PublicKey, PrivateKey)
generateCAKeyPair = let gen = mkStdGen 3
                        (pub, pri, _) = generateKeyPair gen 3 in (pub, pri)


readPubEK :: IO TPM_PUBKEY
readPubEK = do
  handle <- openFile exportEKFileName ReadMode
  pubKeyString <- hGetLine handle
  let pubKey :: TPM_PUBKEY
      pubKey = read pubKeyString
  hClose handle
  return pubKey





{-
process :: LibXenVChan -> IO ()
process chan = do
  ctrlWait chan
  ed :: EvidenceDescriptor <- receive chan
  let ep = measure ed
  send chan ep
  return ()


measure :: EvidenceDescriptor -> EvidencePiece
measure ed = case ed of 
  D0 -> M0 m0Val
  D1 -> M1 m1Val
  D2 -> M2 m2Val
-}
                        


