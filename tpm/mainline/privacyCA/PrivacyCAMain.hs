module Main where
import PrivacyCA
import VChanUtil
import Demo3Shared

import Control.Monad

main :: IO ()
main = do 
  putStrLn "main of PrivacyCA"
  
  {-chan <- server_init attId
  forever $ process chan
-}
  return ()