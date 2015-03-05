module Main where

import CAProtoMain (caEntity_CA)
import ProtoMonad
import ProtoTypes
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
  
  
  return $ ProtoEnv 0 myPri ents pubs 0 0 0 
  
  --return ()

--main = attCommInit [1,2]

main :: IO ()
main = do 
  putStrLn "Main of entity CA"
  env <- caCommInit 3 -- [appId, caId] 
  eitherResult <- runProto caEntity_CA env
  case eitherResult of
    Left s -> putStrLn $ "Error occured: " ++ s
    Right _ -> putStrLn $ "Completed successfully" -- ++ (show resp)
  
  
  {-let as = [ANonce empty, ANonce empty, ACipherText empty]
      asCipher = genEncrypt (fst generateAKeyPair) as
      as' = genDecrypt (snd generateAKeyPair) asCipher
  putStrLn $ show $ as' -}
  return () 