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
import Data.Bits
import Data.ByteString (ByteString, pack, append, empty)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Data.Maybe
import qualified Data.Aeson as DA

prompt:: IO Int
prompt= loop
      where loop = do putStrLn "Which Domain ID is the Appraiser?"
                      input <- getLine
                      case reads input of
                        [(id,_)] -> return id
                        _     -> do putStrLn "Error: Please Enter a Number."
                                    loop
                                    
                                    
measurePrompt :: IO Int
measurePrompt = loop
      where loop = do putStrLn "Which Domain ID is the Measurer?"
                      input <- getLine
                      case reads input of
                        [(id,_)] -> return id
                        _     -> do putStrLn "Error: Please Enter a Number."
                                    loop

main :: IO ()
main = do
  appraiserID <- prompt
  chan <- server_init appraiserID
  req <- receiveRequest chan
  resp <- mkResponse req
  sendResponse chan resp
  return ()

-- Attestation primitives  
   
mkResponse :: Request -> IO Response
mkResponse r  = do
  measurerID <- measurePrompt
  chan <- client_init measurerID
  eList <- mapM (getEvidencePieceFromChan chan) (evidenceDescriptorList_DesiredEvidence
                                                        (desiredEvidence_Request r))
  let nonce = nonce_Request r
      evPack = signEvidence (Evidence eList) nonce
      quote = mkSignedTPMQuote (tpm_Request r) nonce
      hash = doHash $ LB.toStrict $ (jsonEncode eList) `LB.append`
                                    (jsonEncode nonce)--ePack eList (nonceRequest r)
      quoPack = signQuote quote (B.unpack hash)
        
  return (Response evPack quoPack)

getEvidencePieceFromChan :: LibXenVChan -> EvidenceDescriptor -> IO EvidencePiece
getEvidencePieceFromChan chan ed = do
  putStrLn $ "\n" ++ "Attestation Agent Sending: " ++ (show (jsonEncode (EvidenceDescriptorW ed)))
  logger <- createLogger
  sendChunkedMessageByteString logger chan (LB.toStrict  (jsonEncode (EvidenceDescriptorW ed)))
  --send chan $ encode (wrapED ed)
  ctrlWait chan
  logger <- createLogger
  bytes <- readChunkedMessageByteString logger chan
  let evidenceP = getEvidencePiece $ fromJust (jsonDecode (LB.fromStrict bytes) :: Maybe WrappedData) --TODO:  error handling
  putStrLn $ "Received: " ++ (show evidenceP)
  return evidenceP
  
receiveRequest :: LibXenVChan -> IO Request
receiveRequest chan = do
  ctrlWait chan
  logger <- createLogger
  bytes <-  readChunkedMessageByteString logger chan
  let  res = Just $ getRequest (fromJust ((jsonDecode (LB.fromStrict bytes)) :: Maybe WrappedData))
  --res :: Shared <- receive chan
  case res of
    Just req -> do
      putStrLn $ "\n" ++ "Attester Received: " ++ show res ++ "\n"
      return req
    otherwise -> error requestReceiveError
      
sendResponse :: LibXenVChan -> Response-> IO ()   
sendResponse chan resp = do
  putStrLn $ "Attester Sending: " ++ (show $ resp) ++ "\n"
  logger <- createLogger
  sendChunkedMessageByteString logger chan (LB.toStrict  (jsonEncode (ResponseW resp)))
  --send chan $ Attestation resp
  return () 

signQuote :: Quote -> Hash -> QuotePackage
signQuote quote hash =
  case sign Nothing md5 pri res of
         Left err -> error $ show err
         Right signature -> QuotePackage quote hash (B.unpack signature)
 where res =  LB.toStrict $ (jsonEncode quote) `LB.append` (jsonEncode hash)

signEvidence :: Evidence -> Nonce -> EvidencePackage
signEvidence e n =
  case sign Nothing md5 pri res of
         Left err -> error $ show err
         Right signature -> EvidencePackage e n (B.unpack signature)
         
   where res = LB.toStrict $ (DA.encode e) `LB.append` (DA.encode n) --ePack e n

mkSignedTPMQuote :: TPMRequest -> Nonce -> Quote
mkSignedTPMQuote mask nonce =
    let pcrs' = pcrSelect mask in
       -- quote = (pcrs', nonce) in
      case sign Nothing md5 pri $ LB.toStrict $ ((DA.encode pcrs') `LB.append` (DA.encode nonce)) of
         Left err -> error $ show err
         Right signature -> Quote pcrs' nonce (B.unpack signature) 
                 
                            
  -- PCR primitives
pcrsLocal :: [PCR]
pcrsLocal = a --b
  where a :: [PCR]
        a = map bit [0..7]

        b :: [PCR]
        b = bit 3 : map bit [1..7]
     

pcrSelect :: TPMRequest -> [PCR]
pcrSelect mask = 
    [ x | (x, n) <- zip pcrsLocal [0..7], testBit mask n] 

-- Crypto primitives
md5 :: HashDescr
md5 = hashDescrMD5

pub :: PublicKey
pri :: PrivateKey
(pri, pub) = getKeys

-- Utility functions to get keys
attKeyFileName :: String
attKeyFileName = "attKeys.txt"

getKeys :: (PrivateKey, PublicKey)
getKeys = unsafePerformIO readKeys

getPriKey :: PrivateKey
getPriKey = fst getKeys

getPubKey :: PublicKey
getPubKey = snd getKeys

readKeys :: IO (PrivateKey, PublicKey)
readKeys = do
  handle <- openFile attKeyFileName ReadMode
  priString <- hGetLine handle
  pubString <- hGetLine handle
  let pri :: PrivateKey
      pri = read priString
      pub :: PublicKey
      pub = read pubString
  hClose handle
  return (pri, pub)
  

--Error messages(only for debugging, at least for now)
requestReceiveError :: String
requestReceiveError = "Attester did not receive a Request as expected"
