{-# LANGUAGE ScopedTypeVariables #-}

module PrivacyCA where

import TPM
import Demo3Shared
import VChanUtil

import Data.Binary
import Data.Bits
import Data.ByteString.Lazy (ByteString, cons, empty, pack, toStrict, fromStrict)
import qualified Data.ByteString as B (ByteString, pack)
--import Codec.Crypto.RSA hiding (sign)
import System.Random
import System.IO
import Crypto.Cipher.AES
--import Codec.Crypto.AES
--withOpenSSL


caProcess :: PlatformID -> IO ()
caProcess = process receiveCARequest sendCAResponse mkCAResponse

{-
caProcess :: LibXenVChan -> IO ()
caProcess chan = do
  --ctrlWait chan
  req <- receiveCARequest chan
  resp <- mkCAResponse req
  sendCAResponse chan resp
  return () 
-}


receiveCARequest :: LibXenVChan -> IO (Either String CARequest)
receiveCARequest chan = do
			  eitherShared <- receiveShared chan
			  case (eitherShared) of
			   (Left err) -> return (Left err)
			   (Right (WCARequest caReq)) -> do 
                             putStrLn $ "Received: " ++ (show caReq)
                             return (Right caReq)
			   (Right x) -> return (Left ("I wasn't supposed to get this!. I expected a 'CARequest' but I received this: " ++ (show x)))




{-
receiveCARequest :: LibXenVChan -> IO CARequest
receiveCARequest chan = do
  ctrlWait chan
  req :: CARequest <- receive chan
  putStrLn $ "\n" ++ "CA Received: "++ (show req) ++ "\n"                   
  return req
-}

  
                  
sendCAResponse :: LibXenVChan -> CAResponse -> IO ()
sendCAResponse chan caResp = do
				sendShared' chan (WCAResponse caResp)
                                putStrLn $ "Sent: " ++ show caResp
				return ()

{-
sendCAResponse :: LibXenVChan -> CAResponse -> IO ()
sendCAResponse chan resp = do
  putStrLn $ "CA Sending: " ++ show resp ++ "\n"
  send chan resp
  return () 
-}


mkCAResponse :: Either String CARequest -> IO CAResponse --Need to check idSig!!!!
mkCAResponse (Right (CARequest id (Signed idContents idSig))) = do
  ekPubKey <- readPubEK
  let iPubKey = identityPubKey idContents
      iDigest = tpm_digest $ encode iPubKey
      asymContents = contents iDigest
      blob = encode asymContents
      encBlob =  tpm_rsa_pubencrypt ekPubKey blob
      
      caPriKey = snd generateCAKeyPair
      caCert = signPack caPriKey iPubKey
      certBytes = encode caCert
      
      strictCert = toStrict certBytes
      encryptedCert = encryptCTR aes ctr strictCert
      enc = fromStrict encryptedCert
      --encryptedSignedAIK = crypt' CTR symKey symKey Encrypt signedAIK  

      --enc = encrypt key certBytes
  return (CAResponse enc encBlob)
 where 
   symKey = 
     TPM_SYMMETRIC_KEY 
     (tpm_alg_aes128) 
     (tpm_es_sym_ctr) 
     key
   
   v:: Word8 
   v = 1
   key = ({-B.-}Data.ByteString.Lazy.pack $ replicate 16 v) 
   --strictKey = toStrict key
   aes = initAES $ toStrict key
   ctr = toStrict key
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
--"One-time use" export function
exportCAPub :: String -> PubKey -> IO ()
exportCAPub fileName pubKey = do
  handle <- openFile fileName WriteMode
  hPutStrLn handle $ show pubKey
  hClose handle
-}

                        


