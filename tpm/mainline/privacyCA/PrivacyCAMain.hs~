module Main where
import Measurer
import VChanUtil
import Demo3Shared

import Control.Monad

main :: IO ()
main = do 
  putStrLn "main of Measurer"
  chan <- server_init attId
  forever $ process chan
  return ()