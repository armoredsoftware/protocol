module Main where
import Appraiser
import VChanUtil
import Demo3Shared
import TPM.Cipher

import Data.Binary

main :: IO ()
main = do 
  putStrLn "START main of Appraiser"
  --chan <- sendPubKeyRequest True
  --pubKey <- receivePubKeyResponse chan
  --publicKey <- tpm_get_rsa_PublicKey pubKey
  
  (pcrSelect, nonce) <- mkTPMRequest ([0..23]::[Word8])
  let mReq = mkMeasureReq [0..2]
      req = (mReq, pcrSelect, nonce)
  chan <- sendRequest req
  response{-@(_, (pubKey, _), _)-} <- receiveResponse chan
  --let publicKey = tpm_get_rsa_PublicKey pubKey
  result <- evaluate req response
  showDemo3EvalResult result
  --TODO:  Evaluation -}
  putStrLn "END main of Appraiser"
  return () 