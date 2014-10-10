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

--tpm_flushspecific tpm handle tpm_rt_key   (use to clean up-unload key)

{-
main :: IO ()
main = do {-do
  takeInit
  chan <- server_init appId
  req <- receiveRequest chan
  resp <- mkResponse req -}
  putStrLn "main of Attestation"
  return ()
-}
  

receivePubKeyRequest :: LibXenVChan -> IO Bool
receivePubKeyRequest chan = do
  ctrlWait chan
  b :: Bool <- receive chan
  putStrLn $ "\n" ++ "Attester Received: " ++ "PubKey Request: " 
                  ++ (show b) ++ "\n"
  return b


sendPubKeyResponse :: LibXenVChan -> PubKeyResponse -> IO ()
sendPubKeyResponse chan iPubKey = do
  putStrLn $ "Attester Sending: " ++ show iPubKey++ "\n"
  send chan iPubKey
  return () 



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
       
testA :: TPM_PUBKEY -> IO ()
testA ekPubKey = do
  (iKeyHandle, _) <- createAndLoadIdentKey
 -- putStrLn "After iKey Loaded"
  {-
  pubKeyShn <- tpm_session_oiap tpm
  iPubKey <- tpm_getpubkey tpm pubKeyShn iKeyHandle iPass
  tpm_session_close tpm pubKeyShn
-}
  iPubKey <- attGetPubKey iKeyHandle iPass
 -- putStrLn "After getting iPubKey"
  

  
  
  
  
  let iDigest = tpm_digest $ encode iPubKey
      asymContents = contents iDigest
      blob = encode asymContents
      encBlob =  tpm_rsa_pubencrypt ekPubKey blob
  
  --putStrLn $ "encblob: " ++ (show encBlob)
  iShn <- tpm_session_oiap tpm
  oShn <- tpm_session_oiap tpm
  --putStrLn "Before activateIDD"
  --putStrLn $ show $ ((fromIntegral $ Data.ByteString.Lazy.length key) :: UINT32)
  result <- tpm_activateidentity tpm iShn oShn iKeyHandle iPass oPass encBlob
  
  --putStrLn "After activateID"
  putStrLn $ show result
  
  tpm_session_close tpm iShn
  tpm_session_close tpm oShn
  
  
  
  
  return ()
  
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
   
   iPass = tpm_digest_pass "i"
   oPass = tpm_digest_pass ownerPass

{-
testFun :: IO ()
testFun = withOpenSSL $ do 
  putStrLn "start of testFun!"
  hasOwner <- tpm_getcap_owner tpm
  when (hasOwner == False) $ do
    (pubkey, _) <- tpm_key_pubek tpm
    tkShn <- tpm_session_oiap tpm
    tpm_takeownership tpm tkShn pubkey oPass sPass
    tpm_session_close tpm tkShn
  sShn <- tpm_session_oiap tpm
  oShn <- tpm_session_osap tpm oPass kty ownerHandle
  putStrLn "here"
  identKey <- tpm_makeidentity tpm sShn oShn key sPass oPass iPass
  putStrLn (show identKey) 
  tpm_session_close tpm sShn --Check True val here!!(use clo?)
  tpm_session_close tpm oShn
  putStrLn "testFun completed"
  return ()
 where key = tpm_key_create_identity tpm_auth_priv_use_only
       kty = tpm_et_xor_owner
       ownerHandle = (0x40000001 :: Word32)
       oPass = tpm_digest_pass ownerPass
       sPass = tpm_digest_pass srkPass
       iPass = tpm_digest_pass "i"
-}


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
       --oKty = tpm_et_xor_owner
       kty = tpm_et_xor_keyhandle
       --ownerHandle = (0x40000001 :: Word32)
       --oPass = tpm_digest_pass ownerPass
       sPass = tpm_digest_pass srkPass
      -- iPass = tpm_digest_pass "i"
       sigPass = tpm_digest_pass "s"

createAndLoadIdentKey :: IO (TPM_KEY_HANDLE, Signature)
createAndLoadIdentKey = do
  
  {-
  sigKeyShn <- tpm_session_osap tpm sPass kty tpm_kh_srk
  sigKey <- tpm_make_signing tpm sigKeyShn tpm_kh_srk sigPass
  
  tpm_session_close tpm sigKeyShn
  --putStrLn "sig TPM_KEY created"
-}

  
  sShn <- tpm_session_oiap tpm
  oShn <- tpm_session_osap tpm oPass oKty ownerHandle
  (identKey, iSig) <- tpm_makeidentity tpm sShn oShn key sPass iPass iPass
  tpm_session_close tpm sShn --Check True val here!!(use clo?)
  tpm_session_close tpm oShn

  
  loadShn <- tpm_session_oiap tpm
  
  iKeyHandle <- tpm_loadkey2 tpm loadShn tpm_kh_srk identKey sPass
  --sKeyHandle <- tpm_loadkey2 tpm loadShn tpm_kh_srk sigKey sPass
  tpm_session_close tpm loadShn
  --putStrLn "sigKey Loaded"
  
  --return sKeyHandle
  return (iKeyHandle, iSig)
    
 where key = tpm_key_create_identity tpm_auth_never
       oKty = tpm_et_xor_owner
       kty = tpm_et_xor_keyhandle
       ownerHandle = (0x40000001 :: Word32)
       oPass = tpm_digest_pass ownerPass
       sPass = tpm_digest_pass srkPass
       iPass = tpm_digest_pass "i"
       --sigPass = tpm_digest_pass "s"
  


mkResponse :: Request -> CAResponse -> TPM_KEY_HANDLE -> IO Response
mkResponse (desiredE, pcrSelect, nonce) (caCert, actIdInput) iKeyHandle = do
  --measurerID <- measurePrompt
  chan <- client_init meaId
  eList <- mapM (getEvidencePiece chan) desiredE
  --TODO: split these next two into getIdentitySession
  {-sShn <- tpm_session_oiap tpm
  oShn <- tpm_session_osap tpm oPass oKty ownerHandle
  identKey <- tpm_makeidentity tpm sShn oShn key sPass oPass iPass
  tpm_session_close tpm sShn --Check True val here!!(use clo?)
  tpm_session_close tpm oShn
  -}
      
  {-    
  sigKeyShn <- tpm_session_osap tpm sPass kty tpm_kh_srk
  sigKey <- tpm_make_signing tpm sigKeyShn tpm_kh_srk sigPass
  
  tpm_session_close tpm sigKeyShn
  putStrLn "sig TPM_KEY created"
  
  loadShn <- tpm_session_oiap tpm
  --iKeyHandle <- tpm_loadkey2 tpm loadShn tpm_kh_srk identKey sPass
  sKeyHandle <- tpm_loadkey2 tpm loadShn tpm_kh_srk sigKey sPass
  tpm_session_close tpm loadShn
  putStrLn "sigKey Loaded"
-}
  
  --badnonce <- nonce_create
  let evBlob = ePack eList nonce --concat and hash elist and nonce, then sign that blob with AIK(using tpm_sign)
      evBlobSha1 = bytestringDigest $ sha1 evBlob
  
  {-
  sigShn <- tpm_session_oiap tpm
  eSig <- tpm_sign tpm sigShn sKeyHandle sigPass evBlobSha1
  tpm_session_close tpm sigShn
  --putStrLn "evBlob signed"
  CHANGE THIS WHEN READY TO DO REAL SIGN -}
      
  let eSig = empty --TEMPORARY
  
  let evPack = (eList, nonce, eSig)
  

  
  --putStrLn $ "encblob: " ++ (show encBlob)
  iShn <- tpm_session_oiap tpm
  oShn <- tpm_session_oiap tpm
  --putStrLn "Before activateIDD"
  --putStrLn $ show $ ((fromIntegral $ Data.ByteString.Lazy.length key) :: UINT32)
  sessionKey <- tpm_activateidentity tpm iShn oShn iKeyHandle iPass oPass 
                                                      actIdInput
  
  --putStrLn "After activateID"
  putStrLn $ show sessionKey
  
  let keyBytes = tpmSymmetricData sessionKey
      strictKey = toStrict keyBytes
      aes = initAES strictKey
      ctr = strictKey
      
      decryptedCertBytes = decryptCTR aes ctr (toStrict caCert)
      lazy = fromStrict decryptedCertBytes
      decodedCACert = (decode lazy) :: CACertificate
      --putStrLn $ show keyBytes
      --putStrLn $ show strictKey
  --putStrLn $ show lazy
  putStrLn $ show decodedCACert
  
  tpm_session_close tpm iShn
  tpm_session_close tpm oShn
  
  
  --quote = mkSignedTPMQuote desiredPCRs nonce --tpm_quote
      -- hash = doHash $ ePack eList nonce --replace w/ 3 lines above
      --quoPack = signQuote quote hash --tpm_quote does this
  quoteShn <- tpm_session_oiap tpm
  quote <- tpm_quote tpm quoteShn iKeyHandle (TPM_NONCE evBlobSha1) pcrSelect iPass 
  tpm_session_close tpm quoteShn    
  putStrLn "Quote generated"
  
  --return (evPack, quoPack)  
  
  --TODO:  EVICT SIGNING KEY HERE!!??
  tpm_flushspecific tpm iKeyHandle tpm_rt_key  --Evict loaded key
  putStrLn "End of MkResponse"
  return (evPack, decodedCACert, quote)
  

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
  
  
  
  
receiveRequest :: LibXenVChan -> IO Request
receiveRequest chan = do
  ctrlWait chan
  res :: Shared <- receive chan
  case res of
    Appraisal req -> do
      putStrLn $ "\n" ++ "Attester Received: " ++ show res ++ "\n"
      return req
    otherwise -> error requestReceiveError 
    
sendResponse :: LibXenVChan -> Response-> IO ()   
sendResponse chan resp = do
  putStrLn $ "Attester Sending: " ++ show (Attestation resp) ++ "\n"
  send chan $ Attestation resp
  return () 


mkCARequest :: TPM_DIGEST -> TPM_PUBKEY -> Signature -> CARequest
mkCARequest privCALabel iPubKey iSig = {-do 
  pubKeyShn <- tpm_session_oiap tpm
  iPubKey <- tpm_getpubkey tpm pubKeyShn iKeyHandle iPass
  tpm_session_close tpm pubKeyShn
-}
  let idContents = TPM_IDENTITY_CONTENTS privCALabel iPubKey in 
  (attId, (idContents, iSig))
  
 where iPass = tpm_digest_pass "i"


sendCARequest :: CARequest -> IO LibXenVChan
sendCARequest req = do
  --id <-getDomId
  --putStrLn $ "Appraiser Domain id: "++ show appId
  --other <- prompt
  chan <- client_init caId
  putStrLn $ "\n" ++ "Attestation Sending: "++ 
                  "CA Request: " ++ (show req) ++ "\n"
  send chan $ req
  return chan


receiveCAResponse :: LibXenVChan -> IO CAResponse
receiveCAResponse chan =  do
  ctrlWait chan
  res :: CAResponse <- receive chan
  return res
  {-case res of 
    CAResponse (_,_)  ->  do
      putStrLn $ "\n" ++ "Attestation Received: " ++ show res ++ "\n"
      return res
    otherwise ->  error caReceiveError --TODO: error handling?
-}


    
--Error messages(only for debugging, at least for now)
requestReceiveError :: String
requestReceiveError = "Attester did not receive a Request as expected"

caReceiveError :: String
caReceiveError = "Attester did not receive a CAResponse as expected"