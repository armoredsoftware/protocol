{-# LANGUAGE ScopedTypeVariables #-}


import JSONCaster
import qualified Data.Aeson as DA
--vchan library
import VChanUtil
import System.IO

import Demo2Shared
import Data.Binary
import Data.ByteString (ByteString, cons, empty)
import Data.Bits
import Control.Monad
import Data.Maybe
import qualified Data.ByteString.Lazy as LB



prompt:: IO (Int)
prompt= loop
      where loop = do putStrLn "Which Domain ID is the Attester?"
                      input <- getLine
                      case reads input of
                        [(id,_)] -> return id
                        _     -> do putStrLn "Error: Please Enter a Number."
                                    loop
  
  
main :: IO ()
main = do
  measurerID <- prompt
  chan <- server_init measurerID
  forever $ process chan
  return ()

  
process :: LibXenVChan -> IO ()
process chan = do
  
  ctrlWait chan
  logger <- createLogger
  bytes <- readChunkedMessageString logger chan
  let ed = evidenceDescriptor $ fromJust (DA.decode (head (LB.fromChunks (bytes))) :: Maybe EvidenceDescriptorWrapper)
  let ep = LB.toChunks (DA.encode (EPW (measure ed)))
  logger <- createLogger
  sendChunkedMessageString logger chan ep 
  return ()


measure :: EvidenceDescriptor -> EvidencePiece
measure ed = case ed of 
  D0 -> M0 m0Val
  D1 -> M1 m1Val
  D2 -> M2 m2Val
                        


m0Val :: M0Rep
m0Val = cons (bit 0) empty

m1Val :: M1Rep
m1Val = cons (bit 1) empty

m2Val :: M2Rep
m2Val = cons (bit 2) empty
