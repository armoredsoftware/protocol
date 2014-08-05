import JSONCaster
import Demo2Shared
import Data.Bits
import System.Environment
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
evidenceDesW = EDW D1
desiredEv = DesiredEvidence [D0,D1,D2]
evidenceP = M0 [bit 1]



main :: IO ()
main = do
  B.writeFile "./haskell_out_evidenceDescriptorW" (LB.toStrict (jsonEncode evidenceDesW))
  B.writeFile "./haskell_out_desiredEvidence" (LB.toStrict (jsonEncode desiredEv))
  B.writeFile "./haskell_out_evidencePiece" (LB.toStrict (jsonEncode evidenceP))
