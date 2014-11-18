{-# LANGUAGE ScopedTypeVariables #-}

module Attestation  where

import TPM
import VChanUtil
import Demo3Shared

import Data.Bits
import Data.Binary hiding (put, get)
import Data.ByteString.Lazy(ByteString, append, empty, pack, length, toStrict, fromStrict, cons)
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import Control.Monad
import Control.Monad.Trans.State
import Control.Monad.Trans
import Crypto.Cipher.AES
import System.IO
import qualified Data.Text as T hiding (cons, empty)

import OpenSSL (withOpenSSL)
import AttesterCAComm

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

doExportReq :: String -> Request ->  IO ()
doExportReq fileName comp =
                   do handle <- openFile fileName WriteMode
                      hPutStrLn handle $ show comp
                      hClose handle

--attProcess = process receiveRequest sendResponse mkResponse

attProcess :: [Bool] -> Att ()
attProcess bs = do
  apprChan <- getAppChan
  measChan <- getMeaChan
  priCaChan <- getPriChan
  liftIO $ putStrLn "RECEIVING REQUEST..."
  req <- liftIO $ receiveRequest apprChan
  put $ AttState bs measChan apprChan priCaChan
  resp <- mkResponse req
  liftIO $ putStrLn "Sending Response"
  liftIO $ sendResponse apprChan resp
  liftIO $ putStrLn "After Send Response"
  return ()
  
  
mkResponse :: Either String Request -> Att Response
mkResponse (Right req) = do
  
  x@(iKeyHandle, iSig) <- liftIO $ createAndLoadIdentKey

  caChan <- getPriChan
  c1b <- c1
  caCert <-liftIO $  case c1b of 
    True -> getCACert x caChan
    False ->getBadCACert iKeyHandle
                                    
  resp <- mkResponse' req caCert iKeyHandle iPass
  return resp
 where iPass = tpm_digest_pass "i"
       sigPass = tpm_digest_pass "s"
       
getBadCACert :: TPM_KEY_HANDLE -> IO CACertificate
getBadCACert iKeyHandle = do
  pubKey <- attGetPubKey iKeyHandle iPass  --Faking CACert here
  let caPriKey = snd generateBadCAKeyPair
      caCert = signPack caPriKey pubKey
  return caCert
  where oPass = tpm_digest_pass ownerPass
        iPass = tpm_digest_pass "i"
  
getCACert :: (TPM_KEY_HANDLE, ByteString) -> LibXenVChan 
                    -> IO CACertificate
getCACert (iKeyHandle, iSig) caChan = do
  pubKey <- attGetPubKey iKeyHandle iPass
  --sendPubKeyResponse chan pubKey -- TODO:  Maybe send signing pubkey too
  let caRequest = mkCARequest iPass pubKey iSig
  putStrLn "ABOUT TO CONVERSE WITH SCOTTYCA"
  eitherCAResponse <- converseWithScottyCA caRequest
  putStrLn "I MADE IT PAST CONVERSING WITH SCOTTYCA"
  case (eitherCAResponse) of
    (Left err) -> error ("Failed to receive CAResponse. Error was: " ++ 
                                        (show err))
    (Right (CAResponse caCertBytes actIdInput)) -> do
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
          caCert = decodedCACert
      tpm_session_close tpm iShn
      tpm_session_close tpm oShn
      
      return caCert

 where oPass = tpm_digest_pass ownerPass
       iPass = tpm_digest_pass "i"
              
mkResponse' :: Request -> CACertificate -> TPM_KEY_HANDLE 
                        -> TPM_DIGEST -> Att Response
mkResponse' (Request desiredE pcrSelect nonce) caCert 
                     qKeyHandle qKeyPass= do

  c5b <- c5
  c6b <- c6
  c7b <- c7
  eList <- case (and [c5b, c6b, c7b]) of 
    True -> getEvidence desiredE
    False -> getBadEvidence desiredE
                        
                    
  c3b <- c3
  qnonce <- liftIO $ case c3b of 
    True -> return nonce
    False -> nonce_create --badnonce created here
    
  let evBlob = ePack eList qnonce caCert
      evBlobSha1 = bytestringDigest $ sha1 evBlob
  {-        
  liftIO $ putStrLn ("Nonce Length: " ++ (show $ Data.ByteString.Lazy.length $ encode qnonce) )
  
  liftIO $ putStrLn ("Cert Length: " ++ (show $ Data.ByteString.Lazy.length $ encode caCert) )
-}
  c4b <- c4
  liftIO $ case c4b of
    True -> pcrReset --Revert to default PCRS here for good pcr check
    False -> pcrModify "a" --Change PCRS here for bad pcr check
                   
  c2b <- c2
  quote <- liftIO $ case c2b of 
    True -> mkQuote qKeyHandle qKeyPass pcrSelect evBlobSha1
    False -> mkBadQuote pcrSelect evBlobSha1
             
  let eSig = empty --TEMPORARY
      evPack = (EvidencePackage eList qnonce eSig)
  liftIO $ tpm_flushspecific tpm qKeyHandle tpm_rt_key  --Evict loaded key
  liftIO $ putStrLn "End of MkResponse"
  return (Response evPack caCert quote)       
        
 where key = tpm_key_create_identity tpm_auth_priv_use_only
       oKty = tpm_et_xor_owner
       kty = tpm_et_xor_keyhandle
       ownerHandle = (0x40000001 :: Word32)
       oPass = tpm_digest_pass ownerPass
       sPass = tpm_digest_pass srkPass
       iPass = tpm_digest_pass "i"
       sigPass = tpm_digest_pass "s"
       
mkBadQuote :: TPM_PCR_SELECTION -> ByteString -> IO Quote
mkBadQuote pcrSelect exData = do
  pcrComp <- tpm_pcr_composite tpm pcrSelect
  let quoteInfo = TPM_QUOTE_INFO (tpm_pcr_composite_hash $ pcrComp) (TPM_NONCE exData)
      priKey = generateBadQuotePriKey
  {-putStrLn $ "PriKey: " ++ show priKey
  putStrLn $ "Quote blob length : " ++ (show $ Data.ByteString.Lazy.length $ encode quoteInfo) ++ "\n" 
-}
  let qSig = sign priKey quoteInfo
  return (Quote pcrComp qSig)

getEvidence :: DesiredEvidence -> Att [EvidencePiece]  
getEvidence desiredE = do
  chan <- getMeaChan
  eitherElist' <- liftIO $ mapM (getEvidencePiece chan) (desiredE ++ [DONE])
  let eitherElist = init eitherElist'

  let bools = map isAllRight eitherElist
  case and bools of 
    True -> let eList = map extractRight eitherElist in
      return eList
    False -> error "not all evidence gathered successfully"
    
 where 
   isAllRight :: Either String EvidencePiece -> Bool
   isAllRight (Left x) = False
   isAllRight (Right y) = True
       
   extractRight :: Either String a -> a
   extractRight (Right x) = x

getBadEvidence :: DesiredEvidence -> Att [EvidencePiece]  
getBadEvidence desiredE = do
  m0Val <- do c5b <- c5
              case c5b of 
                True -> return $ expectedM0Val --cons (bit 0) empty
                False -> return $ cons (bit 1) empty 

  m1Val <- do c6b <- c6
              case c6b of 
                True -> return $ expectedM1Val -- cons (bit 0) empty
                False -> return $ cons (bit 1) empty

  m2Val <- do c7b <- c7 
              case c7b of 
                True -> return $  expectedM2Val --cons (bit 2) empty
                False -> return $ cons (bit 1) empty 

  return [M0 m0Val, M1 m1Val, M2 m2Val] 

mkQuote :: TPM_KEY_HANDLE -> TPM_DIGEST -> TPM_PCR_SELECTION 
                  -> ByteString -> IO Quote 
mkQuote qKeyHandle qKeyPass pcrSelect exData = do 
   quoteShn <- tpm_session_oiap tpm
   (pcrComp, sig) <- tpm_quote tpm quoteShn qKeyHandle 
                             (TPM_NONCE exData) pcrSelect qKeyPass
   tpm_session_close tpm quoteShn    
   putStrLn "Quote generated"
   return (Quote pcrComp sig)
        
        
pcrModify :: String -> IO TPM_PCRVALUE
pcrModify val = tpm_pcr_extend_with tpm (fromIntegral pcrNum) val      

pcrReset :: IO TPM_PCRVALUE
pcrReset = do
  tot <- tpm_getcap_pcrs tpm
  tpm_pcr_reset tpm (fromIntegral tot) [fromIntegral pcrNum]
  val <- tpm_pcr_read tpm (fromIntegral 23)
  putStrLn $ show val
  return val

pcrNum :: Int
pcrNum = 23
       
isDone :: EvidenceDescriptor -> Bool
isDone ed = case ed of DONE -> True
                       _ -> False
                   
getEvidencePiece :: LibXenVChan -> EvidenceDescriptor -> IO (Either String EvidencePiece)
getEvidencePiece chan ed = 
  case isDone ed of 
    True -> return $ (Right OK)
    False -> do
      putStrLn $ "\n" ++ "Attestation Agent Sending: " ++ show ed
      sendShared' chan (WEvidenceDescriptor ed) 
      eitherSharedEvidence <- receiveShared chan 
      case (eitherSharedEvidence) of
                            (Left err) -> return (Left err)
                            (Right (WEvidencePiece ep)) -> do
                              putStrLn $ "Received: " ++ show ep
                              return (Right (ep))
                            (Right x) -> return (Left ("I expected EvidencePiece but received: " ++ (show x)))

receiveRequest :: LibXenVChan -> IO (Either String Request)
receiveRequest chan = do
		   eithershared <- receiveShared chan
		   case (eithershared) of
			(Left err) -> return (Left err)
			(Right (WRequest r)) -> do putStrLn $ "Received: " ++ 
                                                                                (show r)
                                                   return (Right r)
			(Right x) -> return (Left ("Received correctly, but was an unexpected type. I expected a 'Response' but here is what I received instead: " ++ (show x)))

sendResponse :: LibXenVChan -> Response-> IO ()   
sendResponse chan response = do
				sendShared' chan (WResponse response) 
                                putStrLn $ "Sent: " ++ (show response)
				return ()

mkCARequest :: TPM_DIGEST -> TPM_PUBKEY -> Signature -> CARequest
mkCARequest privCALabel iPubKey iSig = 
  let idContents = TPM_IDENTITY_CONTENTS privCALabel iPubKey in 
  (CARequest attId (Signed idContents iSig))
  
  
sendCARequest :: LibXenVChan -> CARequest -> IO ()
sendCARequest chan careq = sendShared' chan (WCARequest careq) 

receiveCAResponse :: LibXenVChan -> IO (Either String CAResponse)
receiveCAResponse chan = do
			eithershared <- receiveShared chan
		   	case (eithershared) of
				(Left err) 			-> return (Left err)
				(Right (WCAResponse caresp)) 	-> return (Right caresp)
				(Right x) 			-> return (Left ("Received unexpected type. I expected a 'CAResponse' but here is what I received instead: " ++ (show x)))

--Error messages(only for debugging, at least for now)
requestReceiveError :: String
requestReceiveError = "Attester did not receive a Request as expected"

caReceiveError :: String
caReceiveError = "Attester did not receive a CAResponse as expected"

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



expectedM0Val :: M0Rep
expectedM0Val = decodeFromTextL' (T.pack "357893594")
-- "560146190" --cons (bit 0) empty

expectedM1Val :: M1Rep
expectedM1Val = decodeFromTextL' (T.pack "560146190") --cons (bit 0) empty

expectedM2Val :: M2Rep
expectedM2Val = decodeFromTextL' (T.pack "929611828") --cons (bit 2) empty