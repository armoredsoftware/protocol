module Main where
import Appraiser
import VChanUtil
import Demo3Shared
import TPM.Cipher

import Data.Binary
import Control.Monad

--main = do replicateM_ 10 $ main'
main = do 
  putStrLn "START main of Appraiser\n"
  (pcrSelect, nonce) <- mkTPMRequest ([0..23]::[Word8])
  let mReq = mkMeasureReq [0..2]
      req = (Request mReq pcrSelect nonce)
  putStrLn $ show req
  enterP "send Request"
  chan <- sendRequest req
  putStrLn "\nSENT REQUEST TO ATTESTATION AGENT...\n"
  putStrLn "\nRECEIVING RESPONSE..."
  result <- receiveResponse chan
  case (result) of
	(Left err) -> putStrLn ("Error getting response. Error was: " ++ err)
	(Right response) -> do
				result <- evaluate req response
  				showDemo3EvalResult result
                                putStrLn $ "Iteration: " ++ show (1::Int)
  
  mapM_  (appLoop chan) [2..8]
  putStrLn "END main of Appraiser"

appLoop :: LibXenVChan -> Int -> IO () 
appLoop chan i = do 
  (pcrSelect, nonce) <- mkTPMRequest ([0..23]::[Word8])
  let mReq = mkMeasureReq [0..2]
      req = (Request mReq pcrSelect nonce)
  putStrLn "\nSENDING REQUEST TO ATTESTATION AGENT..."
  sendShared' chan (WRequest req)
  putStrLn "\nRECEIVING RESPONSE..."
  result <- receiveResponse chan
  case (result) of
	(Left err) -> putStrLn ("Error getting response. Error was: " ++ err)
	(Right response) -> do
				result <- evaluate req response
  				showDemo3EvalResult result
                                putStrLn $ "Iteration: " ++ show i
