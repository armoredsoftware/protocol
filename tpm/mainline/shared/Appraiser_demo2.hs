{-# LANGUAGE ScopedTypeVariables #-}
 
--our libraries
import Demo2Shared
import JSONCaster
--vchan library
import VChanUtil

-- crypto libraries
import Crypto.Random
import Crypto.PubKey.HashDescr
import Crypto.PubKey.RSA
import Crypto.PubKey.RSA.PKCS15

-- utility libraries
import Control.Exception hiding (evaluate)
import Control.Monad
import Data.Maybe
import Data.Word
import Data.Bits
import Data.ByteString (ByteString, pack, append, empty, cons)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Lazy as M (fromList, lookup, empty)

prompt:: IO (Int)
prompt= loop
      where loop = do putStrLn "Which Domain ID would you like to Appraise?"
                      input <- getLine
                      case reads input of
                        [(id,_)] -> return id
                        _     -> do putStrLn "Error: Please Enter a Number."
                                    loop



main :: IO ()
main = let (mask, nonce) = mkTPMRequest [0..7] 
           mReq = mkMeasureReq [0..2]
           req = Request mReq mask nonce in
  do chan <- sendRequest req
     response <- receiveResponse chan
     let result = evaluate req response in 
       showDemo2EvalResult result

     
-- Appraisal primitives

mkTPMRequest :: [Int] -> (TPMRequest, Nonce)
mkTPMRequest mask =
    let mask' = foldr (\ x word -> word `setBit` x) zeroBits mask in
      (mask', (B.unpack (fst $ cprgGenerate 16 gen)) )
      
mkMeasureReq :: [Int] -> DesiredEvidence
mkMeasureReq xs = DesiredEvidence (map f xs)
 where f :: Int -> EvidenceDescriptor
       f 0 = D0
       f 1 = D1
       f 2 = D2

sendRequest :: Request -> IO (LibXenVChan)
sendRequest req = do
  id <-getDomId
  putStrLn $ "Appraiser Domain id: "++(show id)
  other <- prompt
  chan <- client_init other
  putStrLn $ "\n" ++ "Appraiser Sending: "++(show $ req) ++ "\n"
  logger <- createLogger
  sendChunkedMessageByteString logger chan (LB.toStrict  (jsonEncode (RequestW req)))
  --send chan $ Appraisal req
  return chan

receiveResponse :: LibXenVChan -> IO Response
receiveResponse chan =  do
  ctrlWait chan
  logger <- createLogger
  bytes <- readChunkedMessageByteString logger chan
  let res = Just ( getResponse ( fromJust ( (jsonDecode (LB.fromStrict bytes)) :: Maybe WrappedData)))
  --res :: Shared <- receive chan
  case res of 
    Just response ->  do
      putStrLn $ "\n" ++ "Appraiser Received: " ++ (show res)++ "\n"
      return response
    otherwise ->  throw $ ErrorCall quoteReceiveError --TODO: error handling?
    
    
-- Evaluation
    
type MeasureEval = (EvidenceDescriptor, Bool)
type Demo2EvalResult = (Bool, Bool, Bool, Bool, Bool, Bool, Bool,[MeasureEval])

evaluate :: Request -> Response -> Demo2EvalResult
evaluate request response = --(d, tReq, nonce) ((e, eNonce, eSig), (tpmQuote@((pcrsIn, qNonce), qSig), hashIn, qpSig))
  let pcrs' = pcrSelect (tpm_Request request)
      --quotePackage gets the quotepackage out of the response.
      --quoteQuotePackage gets the quote out of the QuotePackage
      quote = (quote_QuotePackage (quotePackage_Response response))
      --pcrList gets the pcr list out of the quote
      tpmBlob = LB.toStrict ((jsonEncode (pcrList_Quote quote)) `LB.append`
                             (jsonEncode (nonce_Quote quote))) --tPack (pcrsIn, qNonce)
      evidencePkg = evidencePackage_Response response
      eBlob = LB.toStrict $ (jsonEncode (evidence_EvidencePackage evidencePkg)) `LB.append`
                            (jsonEncode (nonce_EvidencePackage evidencePkg))--ePack e eNonce
      qBlob = LB.toStrict $(jsonEncode quote) `LB.append`
                           (jsonEncode (hash_QuotePackage (quotePackage_Response response)))--qPack tpmQuote hashIn
      qpSig = signature_QuotePackage (quotePackage_Response response)
      eSig = signature_EvidencePackage (evidencePackage_Response response)
      qSig = signature_Quote quote
      r1 = verify md5 pub qBlob (B.pack qpSig)
      r2 = verify md5 pub eBlob (B.pack eSig)
      r3 = verify md5 pub tpmBlob (B.pack qSig) 
      r4 = (pcrList_Quote quote) == pcrs'
      r5 = (nonce_Request request) == (nonce_Quote quote)
      r6 = (doHash eBlob) == B.pack (hash_QuotePackage (quotePackage_Response response))
      r7 = (nonce_Request request) == (nonce_EvidencePackage evidencePkg)
      ms =  evaluateEvidence (desiredEvidence_Request request) (evidence_EvidencePackage (
                                                                   evidencePackage_Response response)) in
  (r1, r2, r3, r4, r5, r6, r7, ms)
  
                                            
evaluateEvidence :: DesiredEvidence -> Evidence -> [MeasureEval]
evaluateEvidence ds es = zipWith f (evidenceDescriptorList_DesiredEvidence ds) (evidencePieceList_Evidence es) 
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
                            Nothing -> throw $ ErrorCall (noGolden ++ show id)
                            Just goldEp -> goldEp == ep 
                         
                         
showDemo2EvalResult :: Demo2EvalResult -> IO ()
showDemo2EvalResult (r1, r2, r3, r4, r5, r6, r7, ms) = 
  let rs = [r1, r2, r3, r4, r5, r6, r7] in do
    zipWithM_ f evalStrings rs
    mapM_ g ms
       
 where 
   f :: String -> Bool -> IO ()
   f s b = putStrLn $ s ++ show b
   
   g :: MeasureEval -> IO ()
   g (d, b) = putStrLn $ show d ++ ": " ++ show b


evalStrings :: [String]
evalStrings = [e1, e2, e3, e4, e5, e6, e7]

e1 :: String
e1 = "Quote Package Signature: "
e2 :: String
e2 = "Evidence Package Signature: "  
e3 :: String
e3 = "TPM Signature: "  
e4 :: String
e4 = "PCR values: "
e5 :: String
e5 = "Quote Nonce verified: "
e6 :: String
e6 = "Integrity of evidence package: "  
e7 :: String
e7 = "Evidence Nonce verified: "
e8 :: String
e8 = "Measurement #"


--Golden Values

goldenMap = M.fromList $ zip [0..2] (evidencePieceList_Evidence expectedEvidence)

expectedEvidence :: Evidence
expectedEvidence = Evidence
  [M0 expectedM0Val , M1 expectedM1Val, M2 expectedM2Val]
  
expectedM0Val :: M0Rep
expectedM0Val =  [bit 0 :: Word8]

expectedM1Val :: M1Rep
expectedM1Val = [bit 0 :: Word8]

expectedM2Val :: M2Rep
expectedM2Val = [bit 2 :: Word8]
                   
                   
-- PCR primitives
pcrsExpected :: [PCR]
pcrsExpected = a --b
  where a :: [PCR]
        a= map bit [0..7]

        b :: [PCR]
        b = [(bit 3)] ++ (map bit [1..7])
     

pcrSelect :: TPMRequest -> [PCR]
pcrSelect mask = 
    [ x | (x, n) <- zip pcrsExpected [0..7], testBit mask n]


-- Crypto primitives
md5 :: HashDescr
md5 = hashDescrMD5

gen :: SystemRNG
gen = unsafePerformIO $ liftM cprgCreate createEntropyPool

pub :: PublicKey
pri :: PrivateKey
(pri, pub) = getKeys


--Utility functions to get keys
apprKeysFileName :: String
apprKeysFileName = "apprKeys.txt"

getKeys :: (PrivateKey, PublicKey)
getKeys = unsafePerformIO $ readKeys

getPriKey :: PrivateKey
getPriKey = fst getKeys

getPubKey :: PublicKey
getPubKey = snd getKeys

readKeys :: IO (PrivateKey, PublicKey)
readKeys = do
  handle <- openFile apprKeysFileName ReadMode
  priString <- hGetLine handle
  pubString <- hGetLine handle
  let pri :: PrivateKey
      pri = read priString
      pub :: PublicKey
      pub = read pubString
  hClose handle
  return (pri, pub)
  
  
--Error messages(only for debugging, at least for now)
quoteReceiveError :: String
quoteReceiveError = "Appraiser did not receive a Quote as expected"

noGolden :: String
noGolden = "No Golden Value for measurement #"