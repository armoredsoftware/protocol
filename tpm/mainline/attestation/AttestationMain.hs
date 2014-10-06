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
  testA pubEk
  return ()

  
{-
  chan <- server_init appId
  
  b <- receivePubKeyRequest chan
  case b of 
    True -> do 
      sigKeyHandle <- createAndLoadKey
      pubKey <- attGetPubKey sigKeyHandle
      sendPubKeyResponse chan pubKey
      req <- receiveRequest chan
      resp <- mkResponse req sigKeyHandle
      sendResponse chan resp
      putStrLn "END main of Attestation"
      return ()
    False -> putStrLn "Could not recognize protocol" -- TODO:  Error handling
-}