module AppMain where

import CAProtoMain (caEntity_App)
import ProtoMonad
import ProtoTypes
import ProtoActions
import VChanUtil
import TPMUtil
import Keys
import TPM.Types
import Provisioning(readComp)
--import ProtoTypes(Channel)

import Prelude 
import Data.ByteString.Lazy hiding (putStrLn)
import qualified Data.Map as M
import System.IO
import Codec.Crypto.RSA
import System.Random
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import Data.Binary

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
      nonce = 34
  eitherResult <- runProto (caEntity_App [0,1,2] nonce pcrSelect) env
  case eitherResult of
    Left s -> putStrLn $ "Error occured: " ++ s
    Right  resp@(ev, n, comp, cert@(SignedData aikPub aikSig), qSig) -> do 
      putStrLn "Response received" 
      print resp
                                           
{-main :: IO ()  
main = do 
  putStrLn "main of Appraiser"
  return () -}
  
  {-let as = [ANonce empty, ANonce empty, ACipherText empty]
      asCipher = genEncrypt (fst generateAKeyPair) as
      as' = genDecrypt (snd generateAKeyPair) asCipher
  putStrLn $ show $ as' -}
  return () 
  
evaluate :: Int -> (EvidenceDescriptor, Nonce, TPM_PCR_SELECTION) -> 
            (Evidence, Nonce, TPM_PCR_COMPOSITE, 
             (SignedData TPM_PUBKEY), Signature) -> IO ()
evaluate pId (d, nonceReq, pcrSelect) 
  (ev, nonceResp, pcrComp, cert@(SignedData aikPub aikSig), qSig) = do 
  let caPublicKey = fst generateCAKeyPair
      blobEvidence :: ByteString
      blobEvidence = packImpl [AEvidence ev, ANonce nonceResp,
                               ASignedData $ SignedData ( ATPM_PUBKEY (dat cert)) (sig cert)] --pubKey
      evBlobSha1 =  bytestringDigest $ sha1 blobEvidence
      
      quoteInfo :: TPM_QUOTE_INFO
      quoteInfo = TPM_QUOTE_INFO (tpm_pcr_composite_hash $ pcrComp)                                                        (TPM_NONCE evBlobSha1) 
      
      aikPublicKey = tpm_get_rsa_PublicKey aikPub
      
      r1 = realVerify caPublicKey (encode aikPub) aikSig
      r2 = realVerify aikPublicKey (encode quoteInfo) qSig
      r3 = nonceReq == nonceResp
  goldenPcrComposite <- readComp
  let r4 = pcrComp == goldenPcrComposite
      r5 = ev == [0,1,2]
      
  putStrLn $ "CACert Signature: " ++ (show r1)
  putStrLn $  "Quote Package Signature: " ++ (show r2)  
  putStrLn $ "Nonce: " ++ (show r3)
  putStrLn $ "PCR Values: " ++ (show r4)
  if (pId == 1) then putStrLn $ "Evidence: " ++ (show r5) else return ()
  return ()