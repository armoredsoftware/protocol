{-# LANGUAGE ScopedTypeVariables #-}

module Attestation  where

import TPM
import VChanUtil
import Demo3Shared

import Data.Binary
import Data.ByteString.Lazy(ByteString, append, empty)
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import Control.Monad

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


sendPubKeyResponse :: LibXenVChan -> TPM_PUBKEY -> IO ()
sendPubKeyResponse chan pubKey = do
  putStrLn $ "Attester Sending: " ++ show (pubKey) ++ "\n"
  send chan pubKey
  return () 



takeInit :: IO ()
takeInit = do 
  hasOwner <- tpm_getcap_owner tpm
  when (hasOwner == False) $ do
    (pubkey, _) <- tpm_key_pubek tpm
    tkShn <- tpm_session_oiap tpm
    tpm_takeownership tpm tkShn pubkey oPass sPass
    tpm_session_close tpm tkShn
 where oPass = tpm_digest_pass ownerPass
       sPass = tpm_digest_pass srkPass
       
       

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


attGetPubKey :: TPM_KEY_HANDLE -> IO TPM_PUBKEY
attGetPubKey handle = do
  shn <- tpm_session_oiap tpm
  pubKey <- tpm_getpubkey tpm shn handle sigPass
  tpm_session_close tpm shn
  return pubKey

 where sigPass = tpm_digest_pass "s"

createAndLoadKey :: IO TPM_KEY_HANDLE
createAndLoadKey = do
  sigKeyShn <- tpm_session_osap tpm sPass kty tpm_kh_srk
  sigKey <- tpm_make_signing tpm sigKeyShn tpm_kh_srk sigPass
  
  tpm_session_close tpm sigKeyShn
  --putStrLn "sig TPM_KEY created"
  
  loadShn <- tpm_session_oiap tpm
  --iKeyHandle <- tpm_loadkey2 tpm loadShn tpm_kh_srk identKey sPass
  sKeyHandle <- tpm_loadkey2 tpm loadShn tpm_kh_srk sigKey sPass
  tpm_session_close tpm loadShn
  --putStrLn "sigKey Loaded"
  return sKeyHandle
    
 where key = tpm_key_create_identity tpm_auth_priv_use_only
        --oKty = tpm_et_xor_owner
       kty = tpm_et_xor_keyhandle
       ownerHandle = (0x40000001 :: Word32)
       --oPass = tpm_digest_pass ownerPass
       sPass = tpm_digest_pass srkPass
       --iPass = tpm_digest_pass "i"
       sigPass = tpm_digest_pass "s"
  


mkResponse :: Request -> TPM_KEY_HANDLE -> IO Response
mkResponse (desiredE, pcrSelect, nonce) sKeyHandle = do
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
  sigShn <- tpm_session_oiap tpm
  eSig <- tpm_sign tpm sigShn sKeyHandle sigPass evBlobSha1
  tpm_session_close tpm sigShn
  --putStrLn "evBlob signed"
  
  let evPack = (eList, nonce, eSig)
  --quote = mkSignedTPMQuote desiredPCRs nonce --tpm_quote
      -- hash = doHash $ ePack eList nonce --replace w/ 3 lines above
      --quoPack = signQuote quote hash --tpm_quote does this
  quoteShn <- tpm_session_oiap tpm
  quote <- tpm_quote tpm quoteShn sKeyHandle (TPM_NONCE evBlobSha1) pcrSelect sigPass 
  tpm_session_close tpm quoteShn    
  putStrLn "Quote generated"
  
  --return (evPack, quoPack)  
  
  --TODO:  EVICT SIGNING KEY HERE!!??
  tpm_flushspecific tpm sKeyHandle tpm_rt_key
  putStrLn "End of MkResponse"
  return (evPack, quote)

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


    
--Error messages(only for debugging, at least for now)
requestReceiveError :: String
requestReceiveError = "Attester did not receive a Request as expected"