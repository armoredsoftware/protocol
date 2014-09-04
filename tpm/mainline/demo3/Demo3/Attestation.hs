{-# LANGUAGE ScopedTypeVariables #-}

module Demo3.Attestation where

import TPM
import VChanUtil
import Demo3.Demo3Shared

import Data.Binary

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
  --makeIdentity(get key handle, maybe use sepearte function to build and load)
  --let evPack = signEvidence eList nonce --concat and hash elist and nonce, then sign that blob with AIK(using tpm_sign)
  --quote = mkSignedTPMQuote desiredPCRs nonce --tpm_quote
      -- hash = doHash $ ePack eList nonce --replace w/ 3 lines above
      --quoPack = signQuote quote hash --tpm_quote does this
        
  --return (evPack, quoPack)  
  return (undefined, undefined)

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
    
{-
ePack :: Evidence -> Nonce -> ByteString
ePack e n = ePack' e `append` n

--This is where we will need to convert measurement type to ByteString
-- if it is something else.  see comment below
ePack' :: Evidence -> ByteString
ePack'  = foldr f empty 
  where f (M0 x) y = x `append` y -- (i.e. (toByteString x) `append` y )
        f (M1 x) y = x `append` y
        f (M2 x) y = x `append` y  
-}
    
--Error messages(only for debugging, at least for now)
requestReceiveError :: String
requestReceiveError = "Attester did not receive a Request as expected"