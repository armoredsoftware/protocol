module AppMain where

import CAProtoMain (caEntity_App)
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

appCommInit :: Channel -> Int -> IO ProtoEnv
appCommInit attChan pId = do
  --attChan <- client_init domid
  let myInfo = EntityInfo "Appraiser" 22 attChan
      attInfo = EntityInfo "Attester" 22 attChan
      mList = [(0, myInfo), (1, attInfo)]
      ents = M.fromList mList
      myPri = snd $ generateAKeyPair
      attPub = getBPubKey
      pubs = M.fromList [(1,attPub)]
  
  
  return $ ProtoEnv 0 myPri ents pubs 0 0 0 pId

{-
appCommInit :: Int -> IO ProtoEnv
appCommInit domid = do
  attChan <- client_init domid
  let myInfo = EntityInfo "Appraiser" 22 attChan
      attInfo = EntityInfo "Attester" 22 attChan
      mList = [(0, myInfo), (1, attInfo)]
      ents = M.fromList mList
      myPri = snd $ generateAKeyPair
      attPub = getBPubKey
      pubs = M.fromList [(1,attPub)]
  
  
  return $ ProtoEnv 0 myPri ents pubs 0 0 0  -}
  
  --return ()

--main = attCommInit [1,2]

appmain :: Channel -> Int -> IO ()
appmain chan pId = do 
  putStrLn "Main of entity Appraiser"
  env <- appCommInit chan pId -- [appId, caId] --TODO: Need Channel form Paul
  let pcrSelect = mkTPMRequest [0..23]
  --TODO:  Choose protocol based on protoId
  eitherResult <- runProto (caEntity_App [0,1,2] pcrSelect) env
  case eitherResult of
    Left s -> putStrLn $ "Error occured: " ++ s
    Right resp -> putStrLn $ "Response received: " ++ (show resp)
  
  
  {-let as = [ANonce empty, ANonce empty, ACipherText empty]
      asCipher = genEncrypt (fst generateAKeyPair) as
      as' = genDecrypt (snd generateAKeyPair) asCipher
  putStrLn $ show $ as' -}
  return () 