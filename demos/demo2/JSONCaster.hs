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


data WrappedData = EvidenceDescriptorWW {getEvidenceDescriptorW EvidenceDescriptorW} |
                   DesiredEvidenceW {getDesiredEvidence DesiredEvidence} |
                   EvidencePieceW {getEvidencePiece EvidencePiece} |
                   QuoteW {getQuote Quote} |
                   EvidenceW {getEvidence Evidence} |
                   RequestW {getRequest Request} |
                   ResponseW {getResponse Response} |
                   EvidencePackageW {getEvidencePackage EvidencePackage} |
                   QuotePackageW {getQuotePackage quotePackage} deriving (Show)
$(derivejson defaultoptions ''WrappedData)

data EvidenceDescriptorW = EDW {evidenceDescriptor :: EvidenceDescriptor} deriving ( Show)
$(deriveJSON defaultOptions ''EvidenceDescriptor)
$(deriveJSON defaultOptions ''EvidenceDescriptorW)
$(deriveJSON defaultOptions ''DesiredEvidence)
$(deriveJSON defaultOptions ''EvidencePiece)
$(deriveJSON defaultOptions ''Quote)
$(deriveJSON defaultOptions ''Evidence)
$(deriveJSON defaultOptions ''Request)
$(deriveJSON defaultOptions ''Response)
$(deriveJSON defaultOptions ''EvidencePackage)
$(deriveJSON defaultOptions ''QuotePackage)


jsonEncode :: (ToJSON a) => a -> LB.ByteString
jsonEncode = DA.encode

jsonDecode :: (FromJSON a) =>  LB.ByteString -> Maybe a
jsonDecode= DA.decode

