module Main where
import Attestation
import VChanUtil
import Demo3Shared
import TPM

import System.IO

main :: IO ()
main = do
  putStrLn "START main of Attestation"
  pubEk <- takeInit
  --exportEK exportEKFileName pubEk --Export pubEk (for now, manually transmit)
  putStrLn "tpm ownership taken"
  chan <- server_init appId
  
  {-
  b <- receivePubKeyRequest chan
  case b of 
    True -> do 
-}
  (iKeyHandle, iSig) <- createAndLoadIdentKey
  pubKey <- attGetPubKey iKeyHandle iPass
  --sendPubKeyResponse chan pubKey -- TODO:  Maybe send signing pubkey too
  let caRequest = mkCARequest iPass pubKey iSig
  caChan <- sendCARequest caRequest
  caResponse <- receiveCAResponse caChan

  req <- receiveRequest chan
  resp <- mkResponse req caResponse iKeyHandle  --Maybe pass sig key handle
  sendResponse chan resp
  putStrLn "END main of Attestation"
  return ()
  {-
      False -> putStrLn "Could not recognize protocol" -- TODO:  Error handling
-}
  
  where sigPass = tpm_digest_pass "s"
        iPass = tpm_digest_pass "i"

   
        
-- "One-time use" function to export public EK
exportEK :: String -> TPM_PUBKEY -> IO ()
exportEK fileName pubKey = do
  handle <- openFile fileName WriteMode
  hPutStrLn handle $ show pubKey
  hClose handle