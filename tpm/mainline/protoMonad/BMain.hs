module Main where

import ProtoMain (nsEntityB)
import ProtoMonad
import ProtoTypes
import VChanUtil

import Prelude 
import Data.ByteString.Lazy hiding (putStrLn)
import qualified Data.Map as M
import System.IO
import Codec.Crypto.RSA
import System.Random

bcommInit :: Int -> IO ProtoEnv
bcommInit targetDomId = do
  chan <- server_init targetDomId
  let aInfo :: EntityInfo
      aInfo = EntityInfo "A" 11 chan
      ents'' :: M.Map EntityId EntityInfo
      ents'' = M.empty
      ents' = M.insert 1 aInfo ents''
      ents = M.insert 0 (EntityInfo "B" 22 chan) ents'
      myPri = snd $ generateBKeyPair
      bPub = getAPubKey
      pubs' :: M.Map EntityId ProtoTypes.PublicKey
      pubs' = M.empty
      pubs = M.insert 1 (getAPubKey) pubs'
  
  return $ ProtoEnv 0 myPri ents pubs 0 0 0

main :: IO ()
main = do 
  putStrLn "Main of entity B"
  env <- bcommInit 1
  eitherResult <- runProto nsEntityB env
  case eitherResult of
    Left s -> putStrLn $ "Error occured: " ++ s
    Right nonce -> putStrLn $ "Nonce received: " ++ (show nonce)
  return ()




generateBKeyPair :: (Codec.Crypto.RSA.PublicKey, Codec.Crypto.RSA.PrivateKey)
generateBKeyPair = let 
  gen = mkStdGen 11 -- used 3 for A
  (pub, pri, _) = generateKeyPair gen 2048 in (pub, pri)
                                              

getAPubKey :: Codec.Crypto.RSA.PublicKey
getAPubKey =  let 
  gen = mkStdGen 3
  (pub, _, _) = generateKeyPair gen 2048 in pub