module Main where
import Appraiser
import VChanUtil
import Demo3Shared


import Data.Binary

main :: IO ()
main = do 
  putStrLn "START main of Appraiser"
  (pcrSelect, nonce) <- mkTPMRequest ([0..23]::[Word8])
  let mReq = mkMeasureReq [0..2]
      req = (mReq, pcrSelect, nonce)
  chan <- sendRequest req
  response <- receiveResponse chan
  --TODO:  Evaluation -}
  putStrLn "END main of Appraiser"
  return () 