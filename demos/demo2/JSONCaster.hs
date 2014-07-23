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
data DesiredEvidenceWrapper = DEW {desiredEvidence :: DesiredEvidence} deriving (Show)
data EvidenceDescriptorWrapper = EDW {evidenceDescriptor :: EvidenceDescriptor} deriving ( Show)
data EvidencePieceWrapper = EPW {evidencePiece :: EvidencePiece2} deriving (Show)

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
$(deriveJSON defaultOptions ''EvidenceDescriptorWrapper)
$(deriveJSON defaultOptions ''DesiredEvidenceWrapper)
$(deriveJSON defaultOptions ''EvidencePieceWrapper)
$(deriveJSON defaultOptions ''EvidencePiece2)

--(deriveJSON defaultOptions ''M0Rep)
testE2 =createEvidencePiece2 (M0 (Prelude.head (LB.toChunks (LBC.pack "This is the content."))))
