module Main where
import Attestation
import VChanUtil
import Demo3Shared
import TPM
import qualified Data.ByteString.Lazy as L

import System.IO
import Control.Monad
import Control.Monad.Trans

main :: IO ()
main = do
  putStrLn "START main of Attestation"
  --putStrLn "OPENING CHAN(Server chan for Appraiser)"
  apprChan <- server_init appId
  measChan <- client_init meaId
  priCaChan <- client_init caId
  
  forever $ runAtt attLoop $ AttState checksInit measChan apprChan priCaChan True
  
  putStrLn "CLOSING CHAN"
  close apprChan
  return ()

attLoop :: Att ()
attLoop = do 
  liftIO takeInit 

  mapM_ (attProcess) 
              (map buildX ( map listUp [0..7]))

--Helper Functions...
checksInit :: [Bool]
checksInit = replicate 7 True

listUp :: Int -> [Int]
listUp x = [x]

buildX :: [Int] -> [Bool]
buildX [] = []
buildX xs = buildX' allTrue xs
 where  allTrue = replicate numChecks True
        numChecks = 7
	buildX' :: [Bool] -> [Int] -> [Bool]
	buildX' inits xs
		| (null xs) = inits
		| otherwise = let x = head xs
				  xs' = tail xs
				  new = updateFalse x inits in
			        		buildX' new xs'
   
--One-time use exports...        
exportEKBytes :: String -> TPM_PUBKEY -> IO ()
exportEKBytes fileName pubKey = do
  let (TPM_STORE_PUBKEY bs) = tpmPubKeyData pubKey
  L.writeFile fileName bs
        

exportEK :: String -> TPM_PUBKEY -> IO () --One-time use func to export pubEK
exportEK fileName pubKey = do
  handle <- openFile fileName WriteMode
  hPutStrLn handle $ show pubKey
  hClose handle
  
  --pubEk <- takeInit
  --putStrLn $ show pubEk
  --let fileName = "pubEkBytes.txt"
  --exportEKBytes fileName pubEk
  --exportEK exportEKFileName pubEk --Export pubEk (for now, manually transmit)
  --putStrLn "tpm ownership taken"
  --chan <- server_init appId
  
  
  
  
  
  
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
