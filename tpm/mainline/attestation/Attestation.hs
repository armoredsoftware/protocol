{-# LANGUAGE ScopedTypeVariables #-}

module Attestation  where

import TPM
import VChanUtil
import Demo3Shared

import Data.Binary
import Data.ByteString.Lazy(ByteString, append, empty, pack, length, toStrict, fromStrict)
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import Control.Monad

import Crypto.Cipher.AES

import OpenSSL (withOpenSSL)
--withOpenSSL 


takeInit :: IO TPM_PUBKEY
takeInit = do 
  tpm_forceclear tpm
  {-sOwner <- tpm_getcap_owner tpm
  when (hasOwner == False) $ do
-}
  (pubkey, _) <- tpm_key_pubek tpm
  --putStrLn $ "Public EK: " ++ show pubkey
  tkShn <- tpm_session_oiap tpm
  tpm_takeownership tpm tkShn pubkey oPass sPass
  tpm_session_close tpm tkShn
  return pubkey
 where oPass = tpm_digest_pass ownerPass
       sPass = tpm_digest_pass srkPass
       

attGetPubKey :: TPM_KEY_HANDLE -> TPM_DIGEST -> IO TPM_PUBKEY
attGetPubKey handle pass = do
  shn <- tpm_session_oiap tpm
  pubKey <- tpm_getpubkey tpm shn handle pass
  tpm_session_close tpm shn
  return pubKey

createAndLoadSigKey :: IO TPM_KEY_HANDLE
createAndLoadSigKey = do
  sigKeyShn <- tpm_session_osap tpm sPass kty tpm_kh_srk
  sigKey <- tpm_make_signing tpm sigKeyShn tpm_kh_srk sigPass
  tpm_session_close tpm sigKeyShn
  --putStrLn "sig TPM_KEY created"

  loadShn <- tpm_session_oiap tpm
  sKeyHandle <- tpm_loadkey2 tpm loadShn tpm_kh_srk sigKey sPass
  tpm_session_close tpm loadShn
  --putStrLn "sigKey Loaded"
  return sKeyHandle

 where key = tpm_key_create_identity tpm_auth_never
       kty = tpm_et_xor_keyhandle
       sPass = tpm_digest_pass srkPass
       sigPass = tpm_digest_pass "s"

createAndLoadIdentKey :: IO (TPM_KEY_HANDLE, Signature)
createAndLoadIdentKey = do
  sShn <- tpm_session_oiap tpm
  oShn <- tpm_session_osap tpm oPass oKty ownerHandle
  (identKey, iSig) <- tpm_makeidentity tpm sShn oShn key sPass iPass iPass {-pass CALabelDigest here instead of iPass eventually?-}
  tpm_session_close tpm sShn --Check True val here!!(use clo?)
  tpm_session_close tpm oShn

  loadShn <- tpm_session_oiap tpm
  iKeyHandle <- tpm_loadkey2 tpm loadShn tpm_kh_srk identKey sPass
  tpm_session_close tpm loadShn
  --putStrLn "identKey Loaded"
  return (iKeyHandle, iSig)
    
 where key = tpm_key_create_identity tpm_auth_never
       oKty = tpm_et_xor_owner
       kty = tpm_et_xor_keyhandle
       ownerHandle = (0x40000001 :: Word32)
       oPass = tpm_digest_pass ownerPass
       sPass = tpm_digest_pass srkPass
       iPass = tpm_digest_pass "i"

mkResponse :: Request -> CAResponse -> TPM_KEY_HANDLE -> IO Response
mkResponse (Request desiredE pcrSelect nonce) (CAResponse caCertBytes actIdInput) iKeyHandle = do

  
  iShn <- tpm_session_oiap tpm
  oShn <- tpm_session_oiap tpm
  sessionKey <- tpm_activateidentity tpm iShn oShn iKeyHandle iPass oPass 
                                                      actIdInput
  putStrLn $ show sessionKey
  
  let keyBytes = tpmSymmetricData sessionKey
      strictKey = toStrict keyBytes
      aes = initAES strictKey
      ctr = strictKey
      
      
      decryptedCertBytes = {-decrypt keyBytes caCertBytes-} decryptCTR aes ctr (toStrict caCertBytes)
      lazy = fromStrict decryptedCertBytes 
      decodedCACert = (decode lazy {-decryptedCertBytes-}) :: CACertificate
  tpm_session_close tpm iShn
  tpm_session_close tpm oShn
  
    --measurerID <- measurePrompt
  chan <- client_init meaId
  eList <- mapM (getEvidencePiece chan) desiredE

  --badnonce <- nonce_create
  let aikPubKey = dat decodedCACert
      evBlob = ePack eList nonce aikPubKey
      evBlobSha1 = bytestringDigest $ sha1 evBlob
  {-
  sigShn <- tpm_session_oiap tpm
  eSig <- tpm_sign tpm sigShn sKeyHandle sigPass evBlobSha1
  tpm_session_close tpm sigShn
  --putStrLn "evBlob signed"
  CHANGE THIS WHEN READY TO DO REAL SIGN -}
  let eSig = empty --TEMPORARY
  let evPack = (EvidencePackage eList nonce eSig)
  
  
  quoteShn <- tpm_session_oiap tpm
  (pcrComp, sig) <- tpm_quote tpm quoteShn iKeyHandle (TPM_NONCE evBlobSha1) 
                               pcrSelect iPass 
  let quote' = (Quote pcrComp sig)
  tpm_session_close tpm quoteShn    
  putStrLn "Quote generated"
  tpm_flushspecific tpm iKeyHandle tpm_rt_key  --Evict loaded key
  putStrLn "End of MkResponse"
  return (Response evPack decodedCACert quote')
  
 where key = tpm_key_create_identity tpm_auth_priv_use_only
       oKty = tpm_et_xor_owner
       kty = tpm_et_xor_keyhandle
       ownerHandle = (0x40000001 :: Word32)
       oPass = tpm_digest_pass ownerPass
       sPass = tpm_digest_pass srkPass
       iPass = tpm_digest_pass "i"
       sigPass = tpm_digest_pass "s"
  
getEvidencePiece :: LibXenVChan -> EvidenceDescriptor -> IO EvidencePiece
getEvidencePiece chan ed = do
  putStrLn $ "\n" ++ "Attestation Agent Sending: " ++ show ed
  send chan ed
  ctrlWait chan
  evidence :: EvidencePiece <- receive chan --TODO:  error handling
  putStrLn $ "Received: " ++ show evidence
  return evidence
  
--receiveRequest :: LibXenVChan -> IO Request
receiveRequest :: LibXenVChan -> IO (Either String Request)
receiveRequest chan = do
		   eithershared <- receiveShared chan
		   case (eithershared) of
			(Left err) -> return (Left err)
			(Right (WRequest r)) -> return (Right r)
			(Right x) -> return (Left ("Received correctly, but was an unexpected type. I expected a 'Response' but here is what I received instead: " ++ (show x)))
--receiveRequest = receiveM attName
{-
receiveRequest :: LibXenVChan -> IO Request
receiveRequest chan = do
  ctrlWait chan
  res :: Shared <- receive chan
  case res of
    Appraisal req -> do
      putStrLn $ "\n" ++ "Attester Received: " ++ show res ++ "\n"
      return req
    otherwise -> error requestReceiveError 
-}
    
sendResponse :: LibXenVChan -> Response-> IO ()   
sendResponse chan response = do
				sendShared' chan (WResponse response) 
				return ()
--sendResponse = sendM attName
{-
sendResponse :: LibXenVChan -> Response-> IO ()   
sendResponse chan resp = do
  putStrLn $ "Attester Sending: " ++ show (Attestation resp) ++ "\n"
  send chan $ Attestation resp
  return () 
-}

mkCARequest :: TPM_DIGEST -> TPM_PUBKEY -> Signature -> CARequest
mkCARequest privCALabel iPubKey iSig = 
  let idContents = TPM_IDENTITY_CONTENTS privCALabel iPubKey in 
  (CARequest attId (Signed idContents iSig))
  
  
sendCARequest :: CARequest -> IO LibXenVChan 
sendCARequest careq = sendShared caId (WCARequest careq) 
--sendCARequest = sendR caId attName

{-
sendCARequest :: CARequest -> IO LibXenVChan
sendCARequest req = do
  chan <- client_init caId
  putStrLn $ "\n" ++ "Attestation Sending: "++ 
                  "CA Request: " ++ (show req) ++ "\n"
  send chan $ req
  return chan
-}


--receiveCAResponse :: LibXenVChan -> IO CAResponse
receiveCAResponse :: LibXenVChan -> IO (Either String CAResponse)
receiveCAResponse chan = do
			eithershared <- receiveShared chan
		   	case (eithershared) of
				(Left err) 			-> return (Left err)
				(Right (WCAResponse caresp)) 	-> return (Right caresp)
				(Right x) 			-> return (Left ("Received unexpected type. I expected a 'CAResponse' but here is what I received instead: " ++ (show x)))
--receiveCAResponse = receiveM attName

{-
receiveCAResponse :: LibXenVChan -> IO CAResponse
receiveCAResponse chan =  do
  ctrlWait chan
  res :: CAResponse <- receive chan --TODO: error handling?
  putStrLn $ "\n" ++ "Attestation Received: "++ (show res) ++ "\n"
  return res
-}



{-
receivePubKeyRequest :: LibXenVChan -> IO Bool
receivePubKeyRequest chan = do
  ctrlWait chan
  b :: Bool <- receive chan
  putStrLn $ "\n" ++ "Attester Received: " ++ "PubKey Request: " 
                  ++ (show b) ++ "\n"
  return b
-}

{-
sendPubKeyResponse :: LibXenVChan -> PubKeyResponse -> IO ()
sendPubKeyResponse chan iPubKey = do
  putStrLn $ "Attester Sending: " ++ show iPubKey++ "\n"
  send chan iPubKey
  return () 
-}

    
--Error messages(only for debugging, at least for now)
requestReceiveError :: String
requestReceiveError = "Attester did not receive a Request as expected"

caReceiveError :: String
caReceiveError = "Attester did not receive a CAResponse as expected"
