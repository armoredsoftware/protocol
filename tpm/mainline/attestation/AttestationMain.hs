module Main where
import Attestation
import VChanUtil
import Demo3Shared
import TPM
import qualified Data.ByteString.Lazy as L

import System.IO
import Control.Monad
import Control.Monad.Trans

checksInit :: [Bool]
checksInit = replicate 7 True

--Attester errors:  tpm_fail  after ~13,500 iterations

--Appraiser errors:
--Appraiser: signature representative out of range   (Only happens when we use a signing key(not identity key) to produce the fake quote signature.  Does not appear to cause problems when the other key is an identity key, and doesn't cause problems if we sign the QUOTE_INFO completely outside of the tpm.)


{-VChan errors:  (Note, all vchan errors eliminated when we keep the same channel open between seperate runs of the protocol.  Still need a more robust way to initiate communication, and to maintain(or re-connect) communication between seperate runs.)

1) transmitChan: vchan init for xs=/local/domain/19/data/serverVchan_20 to domId=20,
vchan to serverExp1 write: Invalid argument

2) Error: Too many open files: libxenvchan_client_init: domId=21, xsPath=/local/domain/21/data/serverVchan_19.

3) transmitChan: vchan init for xs=/local/domain/22/data/serverVchan_19 to domId=19,
Error: Interrupted system call: libxenvchan_client_init: domId=22, xsPath=/local/domain/22/data/serverVchan_19.


4) vchan to serverExp1 write: Inappropriate ioctl for device

-}

listUp :: Int -> [Int]
listUp x = [x]


attLoop :: Att ()
attLoop = do 
  liftIO takeInit 

  --measChan <- getMeaChan
  --apprChan <- getAppChan
  --priCaChan <- getPriChan
  mapM_ (attProcess {-apprChan measChan priCaChan-}) 
              (map buildX ( map listUp [0..7]))


main :: IO ()
main = do
  putStrLn "START main of Attestation"
  --pubEk <- takeInit
  --putStrLn $ show pubEk
  --let fileName = "pubEkBytes.txt"
  --exportEKBytes fileName pubEk
  --exportEK exportEKFileName pubEk --Export pubEk (for now, manually transmit)
  --putStrLn "tpm ownership taken"
  --chan <- server_init appId
  
  putStrLn "OPENING CHAN"
  apprChan <- server_init appId
  measChan <- client_init meaId
  priCaChan <- client_init caId
  
  forever $ runAtt attLoop $ AttState checksInit measChan apprChan priCaChan
  
  putStrLn "CLOSING CHAN"
  close apprChan
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
