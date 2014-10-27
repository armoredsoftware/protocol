{-# LANGUAGE ScopedTypeVariables #-}

module Measurer where

import TPM
import Demo3Shared
import VChanUtil

import Data.Bits
import Data.ByteString.Lazy (ByteString, cons, empty)
--withOpenSSL


meaProcess = process receiveMeaRequest sendMeaResponse measure'

{-
meaProcess :: LibXenVChan -> IO ()
meaProcess chan = do
  ctrlWait chan
  ed :: EvidenceDescriptor <- receive chan
  ep <- measure ed
  send chan ep
  return ()
-}


receiveMeaRequest :: LibXenVChan -> IO (Either String EvidenceDescriptor)
receiveMeaRequest chan = do
			  eitherShared <- receiveShared chan
			  case (eitherShared) of
			   (Left err) -> return (Left ("Measurer received an error: " ++ err))
			   (Right (WEvidenceDescriptor evdes)) -> return (Right evdes)
			   (Right x) -> return (Left ("I wasn't supposed to get this!. I expected an 'EvidenceDescriptor' but I received this: " ++ (show x)))
--receiveMeaRequest = receiveM meaName

sendMeaResponse :: LibXenVChan -> EvidencePiece -> IO ()
sendMeaResponse chan evpiece = do
				sendShared' chan (WEvidencePiece evpiece)
				return ()
--sendMeaResponse = sendM meaName

measure' :: Either String EvidenceDescriptor -> IO EvidencePiece
--measure' (Left err) = do
--			printStrLn "Error: " ++  ("Crap was passed to measure'. Error was: " ++ err)
--			return 
measure' (Right evdes) = do
			    res <- measure evdes
			    return res

measure :: EvidenceDescriptor -> IO EvidencePiece
measure ed = do 
  putStrLn ""
  case ed of 
                    D0 -> return $ M0 m0Val
                    D1 -> return $ M1 m1Val
                    D2 -> return $ M2 m2Val
                        
  
m0Val :: M0Rep
m0Val = cons (bit 0) empty

m1Val :: M1Rep
m1Val = cons (bit 0) empty

m2Val :: M2Rep
m2Val = cons (bit 2) empty
