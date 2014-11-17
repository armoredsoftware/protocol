module Main where
import PrivacyCA
import VChanUtil
import Demo3Shared

import Codec.Crypto.RSA
import Control.Monad
import System.IO


main :: IO ()
main = do 
  putStrLn "main of PrivacyCA"
  {-let publicKey = fst generateCAKeyPair
  exportCAPub exportCAPubFileName publicKey
  -}
  chan <- server_init attId
  forever $ caProcess chan
  close chan
  return ()

exportCAPub :: String -> PubKey -> IO ()
exportCAPub fileName pubKey = do
  handle <- openFile fileName WriteMode
  hPutStrLn handle $ show pubKey
  hClose handle