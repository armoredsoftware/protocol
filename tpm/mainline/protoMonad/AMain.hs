module AMain where

import ProtoMain (nsEntityA)
import ProtoMonad
import ProtoTypes
import VChanUtil

import Prelude 
import Data.ByteString.Lazy hiding (putStrLn)
import qualified Data.Map as M
import System.IO
import Codec.Crypto.RSA
import System.Random

acommInit :: Int -> IO ProtoEnv
acommInit targetDomId = do
  chan <- client_init targetDomId
  let bInfo :: EntityInfo
      bInfo = EntityInfo "B" 22 chan
      ents' :: M.Map EntityId EntityInfo
      ents' = M.empty
      ents = M.insert 1 bInfo ents'
      myPri = snd $ generateAKeyPair
      bPub = getBPubKey
      pubs' :: M.Map EntityId ProtoTypes.PublicKey
      pubs' = M.empty
      pubs = M.insert 1 (getBPubKey) pubs'
  
  
  return $ ProtoEnv 0 myPri ents pubs 0 0 0


amain :: IO ()
amain = do 
  putStrLn "Main of entity A"
  return ()
  
generateAKeyPair :: (Codec.Crypto.RSA.PublicKey, Codec.Crypto.RSA.PrivateKey)
generateAKeyPair = let 
  gen = mkStdGen 3 -- used 11 for B
  (pub, pri, _) = generateKeyPair gen 2048 in (pub, pri)
                                              
getBPubKey :: Codec.Crypto.RSA.PublicKey
getBPubKey =  let 
  gen = mkStdGen 11 
  (pub, _, _) = generateKeyPair gen 2048 in pub