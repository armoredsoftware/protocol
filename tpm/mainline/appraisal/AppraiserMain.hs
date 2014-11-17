module Main where
import Appraiser
import VChanUtil
import Demo3Shared
import TPM.Cipher

import Data.Binary
import Control.Monad

--main = do replicateM_ 10 $ main'
main = do 
  putStrLn "START main of Appraiser"
  (pcrSelect, nonce) <- mkTPMRequest ([0..23]::[Word8])
  let mReq = mkMeasureReq [0..2]
      req = (Request mReq pcrSelect nonce)
  putStrLn "SENDING REQUEST"
  chan <- sendRequest req
  putStrLn "RECEIVING ON CHAN"
  result <- receiveResponse chan
  case (result) of
	(Left err) -> putStrLn ("Error getting response. Error was: " ++ err)
	(Right response) -> do
				result <- evaluate req response
  				showDemo3EvalResult result
  
  mapM_  (appLoop chan) [0..6]
  putStrLn "END main of Appraiser"

appLoop :: LibXenVChan -> Int -> IO () 
appLoop chan i = do 
  (pcrSelect, nonce) <- mkTPMRequest ([0..23]::[Word8])
  let mReq = mkMeasureReq [0..2]
      req = (Request mReq pcrSelect nonce)
  putStrLn "SENDING REQUEST"
  sendShared' chan (WRequest req)
  putStrLn "RECEIVING ON CHAN"
  result <- receiveResponse chan
  case (result) of
	(Left err) -> putStrLn ("Error getting response. Error was: " ++ err)
	(Right response) -> do
				result <- evaluate req response
  				showDemo3EvalResult result
                                putStrLn $ "Iteration: " ++ show i
