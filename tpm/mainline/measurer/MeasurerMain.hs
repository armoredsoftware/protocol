module Main where
import Measurer
import VChanUtil
import Demo3Shared

import Control.Monad

main = do
  putStrLn "main of Measurer"     
  putStrLn "OPENING CHAN"
  chan <- server_init attId
  forever $ meaLoop chan
  putStrLn "CLOSING CHAN"
  close chan
  return ()

meaLoop :: LibXenVChan -> IO ()
meaLoop chan = do
  meaProcess chan