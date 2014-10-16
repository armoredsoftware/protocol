module Main where
import Appraiser
import VChanUtil
import Demo3Shared
import TPM.Cipher

import Data.Binary

main :: IO ()
main = do 
  putStrLn "START main of Appraiser"
  (pcrSelect, nonce) <- mkTPMRequest ([0..23]::[Word8])
  let mReq = mkMeasureReq [0..2]
      req = (Request mReq pcrSelect nonce)
  chan <- sendRequest req
  response <- receiveResponse chan
  result <- evaluate req response
  showDemo3EvalResult result
  putStrLn "END main of Appraiser"
  return () 