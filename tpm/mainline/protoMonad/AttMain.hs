module AttMain where

import CAProtoMain(caEntity_Att)
import ProtoMonad
import ProtoTypes
import ProtoActions
import VChanUtil
import TPMUtil

import Prelude 
import Data.ByteString.Lazy hiding (putStrLn)
import qualified Data.Map as M
import System.IO
import Codec.Crypto.RSA
import System.Random

attCommInit :: [Int] -> IO ProtoEnv
attCommInit domidS = do
  takeInit --Taking ownership of TPM
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


attmain :: IO ()
attmain = do 
  putStrLn "Main of entity Att"
  env <- attCommInit [1,2]  --[AppId, CaId]
  eitherResult <- runProto caEntity_Att env
  case eitherResult of
    Left s -> putStrLn $ "Error occured: " ++ s
    Right nonce -> putStrLn $ "Nonce received: " ++ (show nonce)
  
  
  {-let as = [ANonce empty, ANonce empty, ACipherText empty]
      asCipher = genEncrypt (fst generateAKeyPair) as
      as' = genDecrypt (snd generateAKeyPair) asCipher
  putStrLn $ show $ as' -}
  return ()
  
generateAKeyPair :: (Codec.Crypto.RSA.PublicKey, Codec.Crypto.RSA.PrivateKey)
generateAKeyPair = let 
  gen = mkStdGen 3 -- used 11 for B
  (pub, pri, _) = generateKeyPair gen 2048 in (pub, pri)
                                              
getBPubKey :: Codec.Crypto.RSA.PublicKey
getBPubKey =  let 
  gen = mkStdGen 11 
  (pub, _, _) = generateKeyPair gen 2048 in pub