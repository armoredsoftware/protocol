module Main where
import PrivacyCA
import VChanUtil
import Demo3Shared

import Control.Monad

main :: IO ()
main = do 
  putStrLn "main of PrivacyCA"
  
  chan <- server_init attId
  req <-receiveCARequest chan
  putStrLn $ show req
  resp <- mkCAResponse req
  putStrLn $ show resp
  sendCAResponse chan resp

  return ()