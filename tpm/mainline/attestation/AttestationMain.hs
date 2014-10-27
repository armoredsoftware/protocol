module Main where
import Attestation
import VChanUtil
import Demo3Shared
import TPM
import qualified Data.ByteString.Lazy as L

import System.IO
import Control.Monad

main :: IO ()
main = do
  putStrLn "START main of Attestation"
  pubEk <- takeInit
  --putStrLn $ show pubEk
  --let fileName = "pubEkBytes.txt"
  --exportEKBytes fileName pubEk
  --exportEK exportEKFileName pubEk --Export pubEk (for now, manually transmit)
  putStrLn "tpm ownership taken"
  --chan <- server_init appId
  
  
  forever $ attProcess appId
  return ()

  {-
  eitherReq <- receiveRequest chan 	  	
  case (eitherReq) of
    (Left err) -> putStrLn ("Failure to receiving request. Failure was: "                                                          ++ err)
   			
    (Right req) -> do
      (iKeyHandle, iSig) <- createAndLoadIdentKey
      pubKey <- attGetPubKey iKeyHandle iPass
      --sendPubKeyResponse chan pubKey -- TODO:  Maybe send signing pubkey too
      let caRequest = mkCARequest iPass pubKey iSig
      caChan <- sendCARequest caRequest
      eitherCAResponse <- receiveCAResponse caChan
      case (eitherCAResponse) of
        (Left err) -> putStrLn ("Failed to receive CAResponse. Error was: " ++ 
                                        (show err))
        (Right caresp) -> do
          resp <- mkResponse' req caresp iKeyHandle  --Maybe pass sig key handle
          putStrLn "After MkResponse"  		
          sendResponse chan resp  		
          putStrLn "END main of Attestation. Attestation Successful!"
          return ()
  {-
      False -> putStrLn "Could not recognize protocol" -- TODO:  Error handling
-}
  
  where sigPass = tpm_digest_pass "s"
        iPass = tpm_digest_pass "i"

-}


   
        
exportEKBytes :: String -> TPM_PUBKEY -> IO ()
exportEKBytes fileName pubKey = do
  let (TPM_STORE_PUBKEY bs) = tpmPubKeyData pubKey
  L.writeFile fileName bs
        
-- "One-time use" function to export public EK
exportEK :: String -> TPM_PUBKEY -> IO ()
exportEK fileName pubKey = do
  handle <- openFile fileName WriteMode
  hPutStrLn handle $ show pubKey
  hClose handle
