import JSONCaster
import Demo2Shared
import Data.Bits
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
evidenceDes = D1
desiredEv = DesiredEvidence [D0,D1,D2]
evidenceP = M0 [bit 1]



main :: IO ()
main = do
  B.writeFile "./haskell_out_evidenceDescriptorW" (LB.toStrict (jsonEncode (EvidenceDescriptorW evidenceDes)))
  B.writeFile "./haskell_out_desiredEvidenceW" (LB.toStrict (jsonEncode (DesiredEvidenceW desiredEv)))
  B.writeFile "./haskell_out_evidencePiece" (LB.toStrict (jsonEncode ( evidenceP)))
