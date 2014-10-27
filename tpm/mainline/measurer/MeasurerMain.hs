module Main where
import Measurer
import VChanUtil
import Demo3Shared

import Control.Monad


main = forever main'
  
main' :: IO ()
main' = do 
  putStrLn "main of Measurer"
  chan <- server_init attId
  meaProcess chan
  putStrLn "HEEEEERRRRRREEEE"
  close chan
  return ()