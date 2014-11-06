{-# LANGUAGE ScopedTypeVariables #-}

module Appraiser where

import TPM
import VChanUtil
import Demo3Shared 
import Provisioning

import Data.Word
import Data.Binary
--import Codec.Crypto.RSA(PublicKey)
import Data.ByteString.Lazy (ByteString, pack, append, empty, cons, fromStrict, length)
import Data.Bits
import Control.Monad
import Data.Digest.Pure.SHA (bytestringDigest, sha1)
import qualified Data.Map.Lazy as M (fromList, lookup, empty)
import qualified Data.ByteString.Char8 as Char8 (pack) --just for testing.
import System.IO

--withOpenSSL
          
mkTPMRequest :: [Word8] -> IO (TPM_PCR_SELECTION, TPM_NONCE)
mkTPMRequest xs = do 
  let max = 24  -- <- tpm_getcap_pcrs tpm  --Maybe add this to assumptions?(24)
  nonce <- nonce_create
  let selection = tpm_pcr_selection max xs
  return (selection, nonce)
                         
mkMeasureReq :: [Int] -> DesiredEvidence
mkMeasureReq = map f 
 where f :: Int -> EvidenceDescriptor
       f 0 = D0
       f 1 = D1
       f 2 = D2
       
sendRequest :: Request -> IO LibXenVChan    
sendRequest req = sendShared attId (WRequest req)

--sendRequest = sendR attId appName 
{-
sendRequest :: Request -> IO LibXenVChan
sendRequest req = do
  putStrLn $ "Appraiser Domain id: "++ show appId
  chan <- client_init attId
  putStrLn $ "\n" ++ "Appraiser Sending: "++ show (Appraisal req) ++ "\n"
  send chan $ Appraisal req
  return chan
-}
  
              
receiveResponse :: LibXenVChan -> IO (Either String Response)--Response
receiveResponse chan = do
                       eithershared <- receiveShared chan
                       case (eithershared) of
			(Left err) -> return (Left err)
			(Right (WResponse resp)) -> return (Right resp)
			(Right x) -> return (Left ("Received unexpected type. I expected a 'Response' but here is what I received instead: " ++ (show x)))
			 
--receiveResponse = receiveM appName
{-
receiveResponse :: LibXenVChan -> IO Response
receiveResponse chan =  do
  ctrlWait chan
  res :: Shared <- receive chan
  case res of 
    Attestation response ->  do
      putStrLn $ "\n" ++ "Appraiser Received: " ++ show res ++ "\n"
      return response
    otherwise ->  error quoteReceiveError --TODO: error handling?
-}

--EVALUATION-------------------------------------

evaluate :: Request -> Response -> IO Demo3EvalResult
evaluate (Request d pcrSelect nonce) 
  (Response (EvidencePackage eList eNonce eSig) caCert@(Signed pubKey caSig)  tpmQuote@(Quote pcrComposite qSig)) = do
  caPublicKey <- readPubCA
  let blobEvidence :: ByteString
      blobEvidence = ePack eList eNonce caCert --pubKey
      evBlobSha1 =  bytestringDigest $ sha1 blobEvidence
      
      quoteInfo :: TPM_QUOTE_INFO
      quoteInfo = TPM_QUOTE_INFO (tpm_pcr_composite_hash $ pcrComposite)                                                        (TPM_NONCE evBlobSha1) 
     {- blobQuote :: ByteString
      blobQuote = encode quoteInfo
-}
      
      aikPublicKey = tpm_get_rsa_PublicKey pubKey
      
      r1 = verify caPublicKey caCert
      signedQuoteInfo = Signed quoteInfo qSig
      
      size = tpm_key_pubsize pubKey
      mod = tpm_key_pubmod pubKey
      modLength = Data.ByteString.Lazy.length $ encode mod
      exp = (tpm_key_pubexp pubKey)
      blobSize = Data.ByteString.Lazy.length $ encode quoteInfo
      
      qSigSize = Data.ByteString.Lazy.length $ qSig
      
      shaBlobLen = Data.ByteString.Lazy.length evBlobSha1
  
      nonceLength = Data.ByteString.Lazy.length $ encode eNonce
  putStrLn $ "Nonce Length: " ++ show nonceLength
  {-
  putStrLn $ "Key Size: " ++ show size  
  putStrLn $ "Key Mod: " ++ show mod
  putStrLn $ "Mod Length: " ++ show modLength
  putStrLn $ "Key Exp: " ++ show exp
  putStrLn $ "Blob Length: " ++ show blobSize
  putStrLn $ "SHA1 Blob Len: " ++ show shaBlobLen
  putStrLn $ "Quote Sig Length: " ++ show qSigSize
-}
  putStrLn "HHHHHHHHHHHHHHHHHHHHHHHHHHHHHH"
  let r2 =  verify aikPublicKey signedQuoteInfo
      r3 = nonce == eNonce
  goldenPcrComposite <- readComp
  let r4 = pcrComposite == goldenPcrComposite
      ms = evaluateEvidence d eList 
  return (r1, r2, r3, r4, ms)
  
  
type Demo3EvalResult = (Bool, Bool, Bool, Bool, [MeasureEval])

showDemo3EvalResult :: Demo3EvalResult -> IO ()
showDemo3EvalResult (r1, r2, r3, r4, ms) = 
  let rs = [r1, r2, r3, r4] in do
    putStrLn ""
    zipWithM_ f evalStrings rs
    mapM_ g ms
       
 where 
   f :: String -> Bool -> IO ()
   f s b = putStrLn $ s ++ show b
   
   g :: MeasureEval -> IO ()
   g (d, b) = putStrLn $ show d ++ ": " ++ show b


evalStrings :: [String]
evalStrings = [e1, e2, e3, e4]

e1 :: String
e1 = "CACert Signature: "
e2 :: String
e2 = "Quote Package Signature: "  
e3 :: String
e3 = "Nonce: "  
e4 :: String
e4 = "PCR values: "


type MeasureEval = (EvidenceDescriptor, Bool) 
    
evaluateEvidence :: DesiredEvidence -> Evidence -> [MeasureEval]
evaluateEvidence  = zipWith f 
 where 
   f :: EvidenceDescriptor -> EvidencePiece -> MeasureEval
   f ed ep = case ed of 
     D0 -> let res = check 0 ep in
       (D0, res)
     D1 -> let res = check 1 ep in
       (D1, res)
     D2 -> let res = check 2 ep in
       (D2, res)
     
       
check :: Int -> EvidencePiece -> Bool
check id ep = let expected = M.lookup id goldenMap in
                          case expected of 
                            Nothing -> error (noGolden ++ show id)
                            Just goldEp -> goldEp == ep 


goldenMap = M.fromList $ zip [0..2] expectedEvidence

expectedEvidence :: Evidence
expectedEvidence = 
  [M0 expectedM0Val , M1 expectedM1Val, M2 expectedM2Val]
  
expectedM0Val :: M0Rep
expectedM0Val = cons (bit 0) empty

expectedM1Val :: M1Rep
expectedM1Val = cons (bit 0) empty

expectedM2Val :: M2Rep
expectedM2Val = cons (bit 2) empty


readPubCA :: IO PubKey
readPubCA = do
  handle <- openFile exportCAPubFileName ReadMode
  pubKeyString <- hGetLine handle
  let pubKey :: PubKey
      pubKey = read pubKeyString
  hClose handle
  return pubKey



{-
sendPubKeyRequest :: Bool -> IO LibXenVChan
sendPubKeyRequest  b = do
  putStrLn $ "Appraiser Domain id: "++ show appId
  chan <- client_init attId
  putStrLn $ "\n" ++ "Appraiser Sending: "++ 
                  "PubKey Request: " ++ (show b) ++ "\n"
  send chan $ b
  return chan
-}

{-
receivePubKeyResponse :: LibXenVChan -> IO PubKeyResponse
receivePubKeyResponse chan = do
  ctrlWait chan
  resp :: PubKeyResponse <- receive chan --TODO: error handling?
  putStrLn $ "\n" ++ "Appraiser Received: " ++ "Pubkey Response: " 
                  ++ show resp ++ "\n"
  return resp
-}



--Error messages(only for debugging, at least for now)
quoteReceiveError :: String
quoteReceiveError = "Appraiser did not receive a Quote as expected"

noGolden :: String
noGolden = "No Golden Value for measurement #"






{-
testRequest :: IO Request
testRequest = do 
  (pcrSelect, nonce) <- mkTPMRequest ([0..23]::[Word8])
  let mReq = mkMeasureReq [0..2]
  return (Request mReq pcrSelect nonce)

testResponse :: IO Response
testResponse = do 
  pubKey <- readPubEK
  comp <- readComp
  putStrLn $ show comp
  let caCert = Signed pubKey m1Val
      quote = Quote comp (fromStrict (Char8.pack "hello")) --m0Val
  
  return $ Response evPack caCert quote

 where evPack = EvidencePackage [M0 m0Val, M1 m1Val] (TPM_NONCE m1Val)                                                     m1Val

-}

m0Val :: M0Rep
m0Val = cons (bit 0) empty

m1Val :: M1Rep
m1Val = cons (bit 1) empty


readPubEK :: IO TPM_PUBKEY
readPubEK = do
  handle <- openFile exportEKFileName ReadMode
  pubKeyString <- hGetLine handle
  let pubKey :: TPM_PUBKEY
      pubKey = read pubKeyString
  hClose handle
  return pubKey
  
{-  
goldenFileName :: String 
goldenFileName= "goldenPcrComosite.txt"

exportEKFileName = "attEKPubKey.txt"
-}


{-
readComp :: IO TPM_PCR_COMPOSITE
readComp = do
  handle <- openFile goldenFileName ReadMode
  compString <- hGetLine handle
  let comp :: TPM_PCR_COMPOSITE
      comp = read compString
  hClose handle
  return comp
-}
