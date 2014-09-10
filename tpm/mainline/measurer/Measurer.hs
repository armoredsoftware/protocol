{-# LANGUAGE ScopedTypeVariables #-}

module Measurer where

import TPM
import Demo3Shared
import VChanUtil

import Data.Bits
import Data.ByteString.Lazy (ByteString, cons, empty)
--withOpenSSL

{-
main :: IO ()
main = do putStrLn "main of Measurer"
-}

process :: LibXenVChan -> IO ()
process chan = do
  ctrlWait chan
  ed :: EvidenceDescriptor <- receive chan
  let ep = measure ed
  send chan ep
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