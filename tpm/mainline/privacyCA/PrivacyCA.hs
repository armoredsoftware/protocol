{-# LANGUAGE ScopedTypeVariables #-}

module PrivacyCA where

import TPM
import Demo3Shared
import VChanUtil

import Data.Binary
import Data.Bits
import Data.ByteString.Lazy (ByteString, cons, empty, pack, toStrict, fromStrict)
import qualified Data.ByteString as B
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
      
      
      caPriKey = snd generateCAKeyPair
      signedAIK = rsassa_pkcs1_v1_5_sign ha_SHA1 caPriKey (encode iPubKey)
      --strictKey = toStrict key
      strictSignedAIK = toStrict signedAIK
      encryptedSignedAIK = encryptCTR aes ctr strictSignedAIK
      enc = fromStrict encryptedSignedAIK
      --encryptedSignedAIK = crypt' CTR symKey symKey Encrypt signedAIK  
      --TODO:  above line will compile if I get AES dependency the same as on my mac
  return (enc, encBlob)
 where 
   symKey = 
     TPM_SYMMETRIC_KEY 
     (tpm_alg_aes128) 
     (tpm_es_sym_ctr) 
     key
   
   v:: Word8 
   v = 1
   key = (Data.ByteString.Lazy.pack $ replicate 16 v) 
   strictKey = toStrict key
   aes = initAES strictKey
   ctr = strictKey
     

   contents dig = TPM_ASYM_CA_CONTENTS symKey dig



readPubEK :: IO TPM_PUBKEY
readPubEK = do
  handle <- openFile exportEKFileName ReadMode
  pubKeyString <- hGetLine handle
  let pubKey :: TPM_PUBKEY
      pubKey = read pubKeyString
  hClose handle
  return pubKey
  
{-
exportCAPub :: String -> PublicKey -> IO ()
exportCAPub fileName pubKey = do
  handle <- openFile fileName WriteMode
  hPutStrLn handle $ show pubKey
  hClose handle
-}





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
                        


