{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module JSONCaster where
   
import Demo2Shared as D2
import Data.Aeson
import qualified Data.Aeson as DA
import Data.Aeson.TH
import Data.Map
import Data.Text
import Data.Word
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.ByteString as B

--data EvidenceDescriptorW = EDW {evidenceDescriptor :: EvidenceDescriptor} deriving ( Show)
{--data WrappedData = EvidenceDescriptorW {getEvidenceDescriptor :: EvidenceDescriptor} |
                   DesiredEvidenceW {desiredEvidence :: DesiredEvidence} |
                   EvidencePieceW {evidencePiece :: EvidencePiece} |
                   QuoteW {quote :: Quote} |
                   EvidenceW {evidence :: Evidence} |
                   RequestW {request :: Request} |
                   ResponseW {response :: Response} |
                   EvidencePackageW {evidencePackage :: EvidencePackage} |
                   QuotePackageW {quotePackage :: QuotePackage} deriving (Show
--}

data WrappedData = EvidenceDescriptorW EvidenceDescriptor
                 | DesiredEvidenceW DesiredEvidence
                 | EvidencePieceW  EvidencePiece
                 | QuoteW  Quote
                 | EvidenceW  Evidence
                 | RequestW  Request
                 | ResponseW Response
                 | EvidencePackageW EvidencePackage
                 | QuotePackageW QuotePackage deriving (Show)
$(deriveJSON defaultOptions ''WrappedData)


$(deriveJSON defaultOptions ''EvidenceDescriptor)
--(deriveJSON defaultOptions ''EvidenceDescriptorW)
--(deriveJSON defaultOptions ''DesiredEvidence)
$(deriveJSON defaultOptions ''EvidencePiece)
$(deriveJSON defaultOptions ''Quote)
--(deriveJSON defaultOptions ''Evidence)
$(deriveJSON defaultOptions ''Request)
$(deriveJSON defaultOptions ''Response)
$(deriveJSON defaultOptions ''EvidencePackage)
$(deriveJSON defaultOptions ''QuotePackage)


jsonEncode :: (ToJSON a) => a -> LB.ByteString
jsonEncode = DA.encode

jsonDecode :: (FromJSON a) =>  LB.ByteString -> Maybe a
jsonDecode= DA.decode

