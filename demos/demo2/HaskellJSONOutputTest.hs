module HaskellJSONOutputTest where

import JSONCaster
import Demo2Shared
import Data.Bits
import System.Environment

evidenceDescriptorW = EDW D1
desiredEvidence = DesiredEvidence [D0,D1,D2]
evidencePiece = M0 [bit 1]



main :: IO ()
main = do
  writeFile "./testoutput" "test stuff goes here!!"
  
