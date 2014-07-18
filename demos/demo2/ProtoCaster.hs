module ProtoCaster where

import qualified Demo2.ProtoRequest    as Proto
import qualified Demo2.DesiredEvidence as Proto
import qualified Demo2.Nonce           as Proto
import qualified Demo2.TPMRequest      as Proto
import qualified Demo2.EvidenceDescriptor as Proto
import qualified Demo2.Nonce           as ProtoN
import qualified Demo2.EvidencePackage as ProtoEP
import qualified Demo2.QuotePackage    as ProtoQP
import qualified Demo2.Quote           as ProtoQ
import qualified Demo2.PCR             as ProtoPCR
import qualified Demo2.EvidencePiece   as Proto
import qualified Demo2.EvidencePiece.Constructor   as Proto
import qualified Demo2.Hash            as ProtoHash
import qualified Demo2.Signature       as ProtoS
import qualified Demo2.Evidence        as Proto
import qualified Demo2.ProtoResponse        as ProtoRes
import Demo2Shared                     as Shared


import Data.Int
import qualified Data.Sequence as S
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import Data.Foldable

requestToProto ::Request -> Proto.ProtoRequest
requestToProto (desiredEv, tpmReq, n) = Proto.ProtoRequest
  (Just (desiredEvidenceToProto desiredEv))
  (Just (tpmRequestToProto tpmReq))
  (Just (nonceToProto n))
                                                         {-- {
       desiredEvidence = desiredEvidenceToProto desiredEv,
       tpmRequest = tpmRequestToProto tpmReq,
       nonce = Just (nonceToProto n)
       }
--}

desiredEvidenceToProto :: DesiredEvidence -> Proto.DesiredEvidence
desiredEvidenceToProto ls = Proto.DesiredEvidence (helper S.empty ls) where
      helper ::S.Seq Proto.EvidenceDescriptor -> DesiredEvidence -> S.Seq Proto.EvidenceDescriptor
      helper s [] = s
      helper s (x:xs) = helper ((S.<|) (evidenceDescToProto x) s) xs

evidenceDescToProto :: EvidenceDescriptor -> Proto.EvidenceDescriptor
evidenceDescToProto x = Proto.EvidenceDescriptor (case x of
                                            D0 -> Just (0 :: Int32)
                                            D1 -> Just (1 :: Int32)
                                            D2 -> Just (2 :: Int32))
evidenceDescToProto _ = Proto.EvidenceDescriptor Nothing

tpmRequestToProto :: TPMRequest -> Proto.TPMRequest
tpmRequestToProto tpmrWord8 =  Proto.TPMRequest (Just (B.singleton tpmrWord8))
tpmRequestToProto _ = Proto.TPMRequest Nothing

nonceToProto :: Nonce -> Proto.Nonce
nonceToProto n = Proto.Nonce (Just (B.fromChunks [n]))
nonceToProto _ = Proto.Nonce Nothing


protoToResponse :: ProtoRes.ProtoResponse -> Response
protoToResponse pr =(protoToEvidencePackage (fromJust $ ProtoRes.evidencePackage pr),
                     protoToQuotePackage (fromJust $ ProtoRes.quotePackage pr))


protoToEvidencePackage :: ProtoEP.EvidencePackage -> EvidencePackage
protoToEvidencePackage pep = (
  (protoToEvidence (fromJust $ ProtoEP.evidence pep)),
  (protoToNonce (fromJust $ ProtoEP.nonce pep)),
  (protoToSignature (fromJust $ ProtoEP.signature pep)))

protoToQuotePackage :: ProtoQP.QuotePackage -> QuotePackage
protoToQuotePackage pqp = (
  (protoToQuote (fromJust $ (ProtoQP.quote pqp))),
  (protoToHash (fromJust $ (ProtoQP.hash pqp))),
  (protoToSignature (fromJust $ (ProtoQP.signature pqp))))

protoToEvidence :: Proto.Evidence -> Evidence
protoToEvidence pe = fmap protoToEvidencePiece (toList (Proto.evidencePiece pe))

protoToNonce :: ProtoN.Nonce -> Nonce
protoToNonce pn = head (B.toChunks $ fromJust $ ProtoN.nonce pn)

protoToSignature :: ProtoS.Signature -> Signature
protoToSignature ps = head (B.toChunks $ fromJust $ ProtoS.signature ps)

protoToQuote :: ProtoQ.Quote -> Quote
protoToQuote pq = (
  ((fmap protoToPCR (toList (ProtoQ.pcr pq))),(protoToNonce (fromJust $ ProtoQ.nonce pq))),
  (protoToSignature (fromJust $ ProtoQ.signature pq)))

                  
protoToHash :: ProtoHash.Hash -> Hash
protoToHash ph = head (B.toChunks (fromJust $ ProtoHash.hash ph))

protoToEvidencePiece :: Proto.EvidencePiece -> EvidencePiece
protoToEvidencePiece pep = case (Proto.constructor pep) of
  (Just Proto.M0) -> Shared.M0 getData
  (Just Proto.M1) -> Shared.M1 getData
  (Just Proto.M2) -> Shared.M2 getData
--  (Just Proto.M3) -> Shared.M3 getData
 where
    getData | (Proto.bytesData pep) /= Nothing = let (Just x) = (Proto.bytesData pep) in (head (B.toChunks x))
        --    | (Proto.doubleData pep) /= Nothing = let (Just x) = (Proto.doubleData pep) in x
          --  | (Proto.stringData pep) /= Nothing = let (Just x) = (Proto.stringData pep) in x


protoToPCR :: ProtoPCR.PCR -> PCR
protoToPCR ppcr = B.head (fromJust $ ProtoPCR.pcr ppcr) 



--for demo2 testing haskell to haskell we need this:

protoToEvidenceDescriptor :: Proto.EvidenceDescriptor -> EvidenceDescriptor
protoToEvidenceDescriptor ped = case (Proto.desc ped) of
                                 (Just 0) -> D0
                                 (Just 1) -> D1
                                 (Just 2) -> D2
