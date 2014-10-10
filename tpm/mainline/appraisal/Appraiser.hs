{-# LANGUAGE ScopedTypeVariables #-}

module Appraiser where

import TPM
import VChanUtil
import Demo3Shared 
import Provisioning

import Data.Word
import Data.Binary
import Codec.Crypto.RSA
import Data.ByteString.Lazy (ByteString, pack, append, empty, cons)
import Data.Bits
import Control.Monad
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
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

receivePubKeyResponse :: LibXenVChan -> IO PubKeyResponse
receivePubKeyResponse chan = do
  ctrlWait chan
  resp :: PubKeyResponse <- receive chan
  putStrLn $ "\n" ++ "Appraiser Received: " ++ "Pubkey Response: " 
                  ++ show resp ++ "\n"
  return resp
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
       
       
sendRequest :: Request -> IO LibXenVChan
sendRequest req = do

  putStrLn $ "Appraiser Domain id: "++ show appId
  chan <- client_init attId
  putStrLn $ "\n" ++ "Appraiser Sending: "++ show (Appraisal req) ++ "\n"
  send chan $ Appraisal req
  return chan
  
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

evaluate :: Request -> Response -> IO Demo3EvalResult
evaluate (d, pcrSelect, nonce) 
  ((eList, eNonce, eSig), (pubKey, caSig),  tpmQuote@(pcrComposite, qSig)) = do
  let blobEvidence :: ByteString
      blobEvidence = ePack eList eNonce
      evBlobSha1 =  bytestringDigest $ sha1 blobEvidence
      
      quoteInfo :: TPM_QUOTE_INFO
      quoteInfo = TPM_QUOTE_INFO (tpm_pcr_composite_hash $ pcrComposite)                                                        (TPM_NONCE evBlobSha1) 
      blobQuote :: ByteString
      blobQuote = encode quoteInfo
      
      publicKey = tpm_get_rsa_PublicKey pubKey
      
      r1 = rsassa_pkcs1_v1_5_verify ha_SHA1 publicKey blobEvidence eSig
      r2 = rsassa_pkcs1_v1_5_verify ha_SHA1 publicKey blobQuote qSig
      r3 = nonce == eNonce
  goldenPcrComposite <- readComp
  let r4 = pcrComposite == goldenPcrComposite
      ms = evaluateEvidence d eList 
  return (r1, r2, r3, r4, ms)
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


showDemo3EvalResult :: Demo3EvalResult -> IO ()
showDemo3EvalResult (r1, r2, r3, r4, ms) = 
  let rs = [r1, r2, r3, r4] in do
    zipWithM_ f evalStrings rs
    mapM_ g ms
       
 where 
   f :: String -> Bool -> IO ()
   f s b = putStrLn $ s ++ show b
   
   g :: MeasureEval -> IO ()
   g (d, b) = putStrLn $ show d ++ ": " ++ show b


evalStrings :: [String]
evalStrings = [e1, e2, e3, e4]

e1 :: String
e1 = "Evidence Package Signature: "
e2 :: String
e2 = "Quote Package Signature: "  
e3 :: String
e3 = "Nonce: "  
e4 :: String
e4 = "PCR values: "







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

{-
goldenPcrComposite :: TPM_PCR_COMPOSITE
goldenPcrComposite = undefined
-}

--Error messages(only for debugging, at least for now)
quoteReceiveError :: String
quoteReceiveError = "Appraiser did not receive a Quote as expected"

noGolden :: String
noGolden = "No Golden Value for measurement #"