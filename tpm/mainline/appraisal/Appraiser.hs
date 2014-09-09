{-# LANGUAGE ScopedTypeVariables #-}

module Appraiser where

import TPM
import VChanUtil
import Demo3Shared 

import Data.Word

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
  --id <-getDomId
  putStrLn $ "Appraiser Domain id: "++ show appId
  --other <- prompt
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


--Error messages(only for debugging, at least for now)
quoteReceiveError :: String
quoteReceiveError = "Appraiser did not receive a Quote as expected"