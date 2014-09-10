{-# LANGUAGE ScopedTypeVariables #-}

module Appraiser where

import TPM
import VChanUtil
import Demo3Shared 

import Data.Word
import Data.Binary
import Codec.Crypto.RSA
import Data.ByteString.Lazy (ByteString, pack, append, empty, cons)
import Data.Bits
import qualified Data.Map.Lazy as M (fromList, lookup, empty)

--withOpenSSL

{-
main :: IO ()
main = do {-do 
  (pcrSelect, nonce) <- mkTPMRequest ([0..23]::[Word8])
  let mReq = mkMeasureReq [0..2]
      req = (mReq, pcrSelect, nonce)
  chan <- sendRequest req
  response <- receiveResponse chan
  --TODO:  Evaluation -}
  putStrLn "main of Appraiser"
  return () 
-}
          
sendPubKeyRequest :: Bool -> IO LibXenVChan
sendPubKeyRequest  b = do
  --id <-getDomId
  putStrLn $ "Appraiser Domain id: "++ show appId
  --other <- prompt
  chan <- client_init attId
  putStrLn $ "\n" ++ "Appraiser Sending: "++ 
                  "PubKey Request: " ++ (show b) ++ "\n"
  send chan $ b
  return chan

receivePubKeyResponse :: LibXenVChan -> IO TPM_PUBKEY
receivePubKeyResponse chan = do
  ctrlWait chan
  pubKey :: TPM_PUBKEY <- receive chan
  putStrLn $ "\n" ++ "Appraiser Received: " ++ "Pubkey Response: " 
                  ++ show pubKey ++ "\n"
  return pubKey
    --False ->  error quoteReceiveError --TODO: error handling?


mkTPMRequest :: [Word8] -> IO (TPM_PCR_SELECTION, TPM_NONCE)
mkTPMRequest xs = do 
  let max = 24 --max  <- tpm_getcap_pcrs tpm
  nonce <- nonce_create
  let selection = tpm_pcr_selection max xs
  return (selection, nonce)
                         
    
mkMeasureReq :: [Int] -> DesiredEvidence
mkMeasureReq = map f 
 where f :: Int -> EvidenceDescriptor
       f 0 = D0
       f 1 = D1
       f 2 = D2
       
       
sendRequest :: Request -> LibXenVChan -> IO ()
sendRequest req chan = do
  --id <-getDomId
  putStrLn $ "Appraiser Domain id: "++ show appId
  --other <- prompt
  --chan <- client_init attId
  putStrLn $ "\n" ++ "Appraiser Sending: "++ show (Appraisal req) ++ "\n"
  send chan $ Appraisal req
  return ()
  
receiveResponse :: LibXenVChan -> IO Response
receiveResponse chan =  do
  ctrlWait chan
  res :: Shared <- receive chan
  case res of 
    Attestation response ->  do
      putStrLn $ "\n" ++ "Appraiser Received: " ++ show res ++ "\n"
      return response
    otherwise ->  error quoteReceiveError --TODO: error handling?









--EVALUATION-------------------------------------

evaluate :: Request -> Response -> PublicKey -> Demo3EvalResult
evaluate (d, pcrSelect, nonce) 
  ( (eList, eNonce, eSig), tpmQuote@(pcrComposite, qSig) ) pubKey = 
  let blobEvidence :: ByteString
      blobEvidence = encode (ePack eList eNonce)
      
      quoteInfo :: TPM_QUOTE_INFO
      quoteInfo = TPM_QUOTE_INFO (tpm_pcr_composite_hash $ pcrComposite)                                                        nonce 
      blobQuote :: ByteString
      blobQuote = encode quoteInfo
      
      r1 = rsassa_pkcs1_v1_5_verify ha_SHA1 pubKey blobEvidence eSig
      r2 = rsassa_pkcs1_v1_5_verify ha_SHA1 pubKey blobQuote qSig
      r3 = nonce == eNonce
      r4 = pcrComposite == goldenPcrComposite
      ms = evaluateEvidence d eList 
  in
  (r1, r2, r3, r4, ms)
                                                                    {-
  let pcrs' = pcrSelect tReq
      tpmBlob = tPack (pcrsIn, qNonce)
      eBlob = ePack e eNonce
      qBlob = qPack tpmQuote hashIn
      r1 = verify md5 pub qBlob qpSig 
      r2 = verify md5 pub eBlob eSig
      r3 = verify md5 pub tpmBlob qSig 
      r4 = pcrsIn == pcrs'
      r5 = nonce == qNonce
      r6 = doHash eBlob == hashIn
      r7 = nonce == eNonce
      ms =  evaluateEvidence d e in
 (r1, r2, r3, r4, r5, r6, r7, ms)
-}



type Demo3EvalResult = (Bool, Bool, Bool, Bool, [MeasureEval])


type MeasureEval = (EvidenceDescriptor, Bool) 
    
evaluateEvidence :: DesiredEvidence -> Evidence -> [MeasureEval]
evaluateEvidence  = zipWith f 
 where 
   f :: EvidenceDescriptor -> EvidencePiece -> MeasureEval
   f ed ep = case ed of 
     D0 -> let res = check 0 ep in
       (D0, res)
     D1 -> let res = check 1 ep in
       (D1, res)
     D2 -> let res = check 2 ep in
       (D2, res)
     
       
check :: Int -> EvidencePiece -> Bool
check id ep = let expected = M.lookup id goldenMap in
                          case expected of 
                            Nothing -> error (noGolden ++ show id)
                            Just goldEp -> goldEp == ep 


goldenMap = M.fromList $ zip [0..2] expectedEvidence

expectedEvidence :: Evidence
expectedEvidence = 
  [M0 expectedM0Val , M1 expectedM1Val, M2 expectedM2Val]
  
expectedM0Val :: M0Rep
expectedM0Val = cons (bit 0) empty

expectedM1Val :: M1Rep
expectedM1Val = cons (bit 0) empty

expectedM2Val :: M2Rep
expectedM2Val = cons (bit 2) empty

goldenPcrComposite :: TPM_PCR_COMPOSITE
goldenPcrComposite = undefined

--Error messages(only for debugging, at least for now)
quoteReceiveError :: String
quoteReceiveError = "Appraiser did not receive a Quote as expected"

noGolden :: String
noGolden = "No Golden Value for measurement #"