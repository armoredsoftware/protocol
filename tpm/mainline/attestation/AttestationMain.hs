module Main where
import Attestation
import VChanUtil
import Demo3Shared
import TPM

main :: IO ()
main = do
  putStrLn "START main of Attestation"
  pubEk <- takeInit
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
      let caRequest = mkCARequest iPass iKeyHandle iSig
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