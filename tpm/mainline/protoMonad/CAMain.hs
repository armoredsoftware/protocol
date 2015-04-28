module CAMain where --Main

import CAProtoMain (caEntity_CA)
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

caCommInit :: Channel -> Int -> IO ProtoEnv
caCommInit attChan pId = do
 -- attChan <- server_init domid
  let myInfo = EntityInfo "CA" 22 attChan
      attInfo = EntityInfo "Attester" 22 attChan
      mList = [(0, myInfo), (1, attInfo)]
      ents = M.fromList mList
      myPri = snd $ generateAKeyPair
      attPub = getBPubKey
      pubs = M.fromList [(1,attPub)] 
  
  
  return $ ProtoEnv 0 myPri ents pubs 0 0 0 pId
  
  
{-
caCommInit :: Int -> IO ProtoEnv
caCommInit domid = do
  attChan <- server_init domid
  let myInfo = EntityInfo "CA" 22 attChan
      attInfo = EntityInfo "Attester" 22 attChan
      mList = [(0, myInfo), (1, attInfo)]
      ents = M.fromList mList
      myPri = snd $ generateAKeyPair
      attPub = getBPubKey
      pubs = M.fromList [(1,attPub)] 
  
  
  return $ ProtoEnv 0 myPri ents pubs 0 0 0  -}
  
  --return ()

--main = attCommInit [1,2]

camain :: Channel -> Int -> IO ()
camain chan pId = do 
  putStrLn "Main of entity CA"
  env <- caCommInit undefined undefined -- [appId, caId]   --TODO: Need Channel form Paul
  --TODO:  choose protocol based on protoId
  eitherResult <- runProto caEntity_CA env
  case eitherResult of
    Left s -> putStrLn $ "Error occured: " ++ s
    Right _ -> putStrLn $ "Completed successfully" -- ++ (show resp)
    --TODO:  Output to Justin's log file here
  
  {-let as = [ANonce empty, ANonce empty, ACipherText empty]
      asCipher = genEncrypt (fst generateAKeyPair) as
      as' = genDecrypt (snd generateAKeyPair) asCipher
  putStrLn $ show $ as' -}
  return () 