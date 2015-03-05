module Keys where

import Codec.Crypto.RSA
import System.Random
import System.IO

import TPM



generateAKeyPair :: (Codec.Crypto.RSA.PublicKey, Codec.Crypto.RSA.PrivateKey)
generateAKeyPair = let 
  gen = mkStdGen 3 -- used 11 for B
  (pub, pri, _) = generateKeyPair gen 2048 in (pub, pri)
                                              
getBPubKey :: Codec.Crypto.RSA.PublicKey
getBPubKey =  let 
  gen = mkStdGen 11 
  (pub, _, _) = generateKeyPair gen 2048 in pub
                                            
                                            
generateCAKeyPair :: (PublicKey, PrivateKey)
generateCAKeyPair = let gen = mkStdGen 3
                        (pub, pri, _) = generateKeyPair gen 2048 in (pub, pri)
                                                                    
                                                                    
exportEKFileName = "ekpub.txt"

readPubEK :: IO TPM_PUBKEY
readPubEK = do
  handle <- openFile exportEKFileName ReadMode
  pubKeyString <- hGetLine handle
  let pubKey :: TPM_PUBKEY
      pubKey = read pubKeyString
  hClose handle
  return pubKey
  
--"One-time use" export function
exportCAPub :: String -> PublicKey -> IO ()
exportCAPub fileName pubKey = do
  handle <- openFile fileName WriteMode
  hPutStrLn handle $ show pubKey
  hClose handle
  
--One-time use func to export pubEK
exportEK :: String -> TPM_PUBKEY -> IO () 
exportEK fileName pubKey = do
  handle <- openFile fileName WriteMode
  hPutStrLn handle $ show pubKey
  hClose handle
