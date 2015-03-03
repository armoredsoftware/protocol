module TPMUtil where

import TPM

import Data.ByteString.Lazy  hiding (putStrLn)
import Data.Word

tpm :: TPMSocket
tpm = tpm_socket "/var/run/tpm/tpmd_socket:0"

ownerPass :: String
ownerPass = "adam"

srkPass :: String
srkPass = ""

aikPass :: String
aikPass = "i"

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
  putStrLn "\n\nTPM OWNERSHIP TAKEN\n"
  return pubkey
 where oPass = tpm_digest_pass ownerPass
       sPass = tpm_digest_pass srkPass
       
makeAndLoadAIK :: IO (TPM_KEY_HANDLE, ByteString)
makeAndLoadAIK = do
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
       iPass = tpm_digest_pass aikPass
       
       
attGetPubKey :: TPM_KEY_HANDLE -> TPM_DIGEST -> IO TPM_PUBKEY
attGetPubKey handle pass = do
  shn <- tpm_session_oiap tpm
  pubKey <- tpm_getpubkey tpm shn handle pass
  tpm_session_close tpm shn
  return pubKey
  
mkQuote :: TPM_KEY_HANDLE -> TPM_DIGEST -> TPM_PCR_SELECTION 
                  -> ByteString -> IO (TPM_PCR_COMPOSITE, ByteString)
mkQuote qKeyHandle qKeyPass pcrSelect exData = do 
   quoteShn <- tpm_session_oiap tpm
   (pcrComp, sig) <- tpm_quote tpm quoteShn qKeyHandle 
                             (TPM_NONCE exData) pcrSelect qKeyPass
   tpm_session_close tpm quoteShn    
   putStrLn $ "\nQuote generated:\n" 
   return (pcrComp, sig)