{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module JSONCaster where

import Demo2Shared as D2
import Data.Aeson
import Data.Aeson.TH
import Data.Map
import Data.Text
import Data.Word
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.ByteString as B
data DesiredEvidenceW = DEW {desiredEvidence :: DesiredEvidence} deriving (Show)
data EvidenceDescriptorW = EDW {evidenceDescriptor :: EvidenceDescriptor} deriving ( Show)
data EvidencePieceW = EPW {evidencePiece2 :: EvidencePiece2} deriving (Show)

data EvidencePiece2 = M02 [Word8]|
                      M12 [Word8] |
                      M22 [Word8] deriving (Show)

createEvidencePiece2 :: EvidencePiece -> EvidencePiece2
createEvidencePiece2 (M0 rep) = M02 (B.unpack rep)
createEvidencePiece2 (M1 rep) = M12 (B.unpack rep)
createEvidencePiece2 (M2 rep) = M22 (B.unpack rep)

ep2ToEp :: EvidencePiece2 -> EvidencePiece
ep2ToEp (M02 rep) = M0 (B.pack rep) 
ep2ToEp (M12 rep) = M1 (B.pack rep)
ep2ToEp (M22 rep) = M2 (B.pack rep)

$(deriveJSON defaultOptions ''EvidenceDescriptor)
$(deriveJSON defaultOptions ''EvidenceDescriptorW)
$(deriveJSON defaultOptions ''DesiredEvidenceW)
$(deriveJSON defaultOptions ''EvidencePieceW)
$(deriveJSON defaultOptions ''EvidencePiece2)

data Testdata = Dat {field :: String} deriving (Show)

$(deriveJSON defaultOptions ''Testdata)
--(deriveJSON defaultOptions ''M0Rep)
testE2 =createEvidencePiece2 (M0 (Prelude.head (LB.toChunks (LBC.pack "This is the content."))))
testED = D0