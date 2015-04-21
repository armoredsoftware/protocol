module Main where

import CAProtoMain (caEntity_Att)
import ProtoMonad
import ProtoTypesA
import ProtoActions
import VChanUtil
import TPMUtil
import Keys

import Prelude 
import Data.ByteString.Lazy hiding (putStrLn)
import qualified Data.Map as M
import System.IO
import Codec.Crypto.RSA
import System.Random

attCommInit :: [Int] -> IO ProtoEnv
attCommInit domidS = do
  ekPub <- takeInit --Taking ownership of TPM
  --exportEK exportEKFileName ekPub
  appChan <- server_init (domidS !! 0)
  caChan <- client_init (domidS !! 1) 
  let myInfo = EntityInfo "Attester" 11 appChan
      appInfo = EntityInfo "Appraiser" 22 appChan
      caInfo = EntityInfo "Certificate Authority" 33 caChan
      mList = [(0, myInfo), (1, appInfo), (2, caInfo)]
      ents = M.fromList mList
      myPri = snd $ generateAKeyPair
      appPub = getBPubKey
      caPub = getBPubKey
      pubs = M.fromList [(1,appPub), (2, caPub)]
  
  
  return $ ProtoEnv 0 myPri ents pubs 0 0 0 

main :: IO ()
main = do 
  putStrLn "Main of entity Attestation"
  env <- attCommInit [1, 4] -- [appId, caId] 
  eitherResult <- runProto caEntity_Att env
  case eitherResult of
    Left s -> putStrLn $ "Error occured: " ++ s
    Right nonce -> putStrLn $ "End of Attestation" -- ++ (show nonce)
  
  return () 
  
