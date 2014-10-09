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
  doExport exportEKFileName pubEk
  --Export pubEk to file here(and transmit to privacyCA)
  putStrLn "tpm ownership taken"
  --testA pubEk
  --return ()



  chan <- server_init appId
  
  b <- receivePubKeyRequest chan
  case b of 
    True -> do 
      (iKeyHandle, iSig) <- createAndLoadIdentKey
      --sigKeyHandle <- createAndLoadSigKey --Maybe have CA send key
      pubKey <- attGetPubKey iKeyHandle iPass
      sendPubKeyResponse chan pubKey  
      -- TODO:  Maybe send signing pubkey here too
      let caRequest = mkCARequest iPass pubKey iSig
      caChan <- sendCARequest caRequest
      caResponse <- receiveCAResponse caChan
      req <- receiveRequest chan
      resp <- mkResponse req caResponse iKeyHandle  --Maybe pass sig key handle too
      sendResponse chan resp
      putStrLn "END main of Attestation"
      return ()
    False -> putStrLn "Could not recognize protocol" -- TODO:  Error handling


  where sigPass = tpm_digest_pass "s"
        iPass = tpm_digest_pass "i"
        
        
        
doExport :: String -> TPM_PUBKEY -> IO ()
doExport fileName pubKey = do
  handle <- openFile fileName WriteMode
  hPutStrLn handle $ show pubKey
  hClose handle