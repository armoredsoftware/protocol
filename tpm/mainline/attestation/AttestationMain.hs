module Main where
import Attestation
import VChanUtil
import Demo3Shared

main :: IO ()
main = do
  putStrLn "START main of Attestation"
  --testFun
  takeInit
  putStrLn "tpm ownership taken"
  chan <- server_init appId
  req <- receiveRequest chan
  resp <- mkResponse req
  sendResponse chan resp
  putStrLn "END main of Attestation"

  return ()