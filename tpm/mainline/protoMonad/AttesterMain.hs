module AttesterMain where --Main

import CAProtoMain (caEntity_Att)
import ProtoMonad
import ProtoTypes
import ProtoActions
import VChanUtil
import TPMUtil
import Keys
import ProtoTypes(Channel)

import Prelude
import Data.ByteString.Lazy hiding (putStrLn)
import qualified Data.Map as M
import System.IO
import Codec.Crypto.RSA
import System.Random

attCommInit :: Channel -> Int -> IO ProtoEnv
attCommInit chan {- domidS -} pId = do
  ekPub <- takeInit --Taking ownership of TPM
  --exportEK exportEKFileName ekPub

  --appChan <- server_init (domidS !! 0)
  --caChan <- client_init (domidS !! 1)
  let appChan = chan -- (domidS !! 0)
      caChan = chan -- (domidS !! 1)
  let myInfo = EntityInfo "Attester" 11 appChan
      appInfo = EntityInfo "Appraiser" 22 appChan
      caInfo = EntityInfo "Certificate Authority" 33 caChan
      mList = [(0, myInfo), (1, appInfo), (2, caInfo)]
      ents = M.fromList mList
  (_,myPri) <- generateAKeyPair
  appPub <- getBPubKey
  caPub <- getBPubKey
  let pubs = M.fromList [(1,appPub), (2, caPub)]


  return $ ProtoEnv 0 myPri ents pubs 0 0 0 pId
{-
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


  return $ ProtoEnv 0 myPri ents pubs 0 0 0  -}

attmain :: Channel -> Int -> IO String
attmain chans pId = do
  putStrLn "Main of entity Attestation"
  env <- attCommInit chans pId --[1, 4]--[appId, caId]--TODO:Need Channels form Paul
  --TODO:  choose protocol based on protoId
  eitherResult <- runProto caEntity_Att env
  let str = case eitherResult of
             Left s -> "Error occured: " ++ s
             Right _ ->"End of Attestation"
  putStrLn str
  return str
