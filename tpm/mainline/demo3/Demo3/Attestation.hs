{-# LANGUAGE ScopedTypeVariables #-}

module Demo3.Attestation where

import TPM
import VChanUtil
import Demo3.Demo3Shared

import Data.Binary
import Data.ByteString.Lazy(ByteString, append, empty)


--withOpenSSL 

--tpm_flushspecific tpm handle tpm_rt_key   (use to clean up-unload key)

attMain :: IO ()
attMain = do
  chan <- server_init appId
  req <- receiveRequest chan
  resp <- mkResponse req
  return ()
  
  
mkResponse :: Request -> IO Response
mkResponse (desiredE, pcrSelect, nonce) = do
  --measurerID <- measurePrompt
  chan <- client_init meaId
  eList <- mapM (getEvidencePiece chan) desiredE
  sShn <- tpm_session_oiap tpm
  oShn <- tpm_session_osap tpm oPass kty ownerHandle
  identKey <- tpm_makeidentity tpm sShn oShn key sPass oPass iPass
  loadShn <- tpm_session_oiap tpm
  iKeyHandle <- tpm_loadkey2 tpm loadShn tpm_kh_srk identKey sPass
  let evBlob = ePack eList nonce --concat and hash elist and nonce, then sign that blob with AIK(using tpm_sign)
  sigShn <- tpm_session_oiap tpm
  eSig <- tpm_sign tpm sigShn iKeyHandle iPass evBlob
  let evPack = (eList, nonce, eSig)
  --quote = mkSignedTPMQuote desiredPCRs nonce --tpm_quote
      -- hash = doHash $ ePack eList nonce --replace w/ 3 lines above
      --quoPack = signQuote quote hash --tpm_quote does this
  quoteShn <- tpm_session_oiap tpm
  quote <- tpm_quote tpm quoteShn iKeyHandle nonce pcrSelect iPass 
      
  --return (evPack, quoPack)  
  return (evPack, quote)

 where key = tpm_key_create_identity tpm_auth_priv_use_only
       kty = tpm_et_xor_owner
       ownerHandle = (0x40000001 :: Word32)
       oPass = tpm_digest_pass ownerPass
       sPass = tpm_digest_pass srkPass
       iPass = tpm_digest_pass "i"
  
  
  
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
    



    
--Error messages(only for debugging, at least for now)
requestReceiveError :: String
requestReceiveError = "Attester did not receive a Request as expected"