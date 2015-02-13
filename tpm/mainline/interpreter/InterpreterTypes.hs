module InterpreterTypes where

import qualified Network.Http.Client as HttpClient
import VChanUtil
import Data.ByteString.Lazy --(ByteString, pack, append, empty, cons, fromStrict, length)



data Entity = Entity {
  entityName :: String,
  entityIp :: Maybe HttpClient.Hostname,
  entityId :: Maybe Int,
  entityNote :: String
}
 deriving (Eq, Show)

data Evidence = String | Bool | Int
type Nonce = ByteString
type PublicKey = ByteString
type PrivateKey = ByteString
type SymmKey = ByteString

data Key = PublicKey | PrivateKey | SymmKey

data Message = Message [ArmoredData]

data RequestItem = ReqNonce Nonce

{-
data RequestDescriptor = RequestDescriptor {
  id :: Int,
  request :: RequestItem,
  expectedResponse :: ResponseItem
  }
                         
data RequestItem = ReqNonce Nonce

data ResponseItem = RespNonce Nonce
-}                   
                   

--Define actions that must be performed on an ArmoredData item before it can be used in the next message.  (i.e.  if we have a nonce and a Public key, and the data item being sent is a signed nonce, we must perform Sign on the nonce before sending it.  This signing can be thought of as a "reduction" step performed on the nonce.)

--Need a way to keep track of data items learned during the protocol run.  How do we label these data items such that we don't mix them up(use the wrong one as a message component, or out of order).  Variables is probably the way to do it.  

--TODO:  Do we need a "Master View" of each protocol description?  Or are we just defining each service from the view of a single entity?



type MetaProp = Int
type EvidenceProp = Int

type MetaAsk = [MetaProp]
type EvidenceAsk = [EvidenceProp]

data MetaGrant = MetaAll | MetaAsk
data EvidenceGrant = EvAll | EvidenceAsk


data ArmoredData =
    Ask MetaAsk EvidenceAsk | Grant MetaGrant EvidenceGrant
  | Nonce
  | AEntity Entity 
  | AKey Key
  | CipherText 
  | HashText
  | SignedData ArmoredData --(Data, Sig)
  | TPM_DATA  
  | Evidence | Certificate | Blob [ArmoredData]

data ArmoredCommand =
    Hash 
  | Crypt    --Symmetric
  | Encrypt --Asymmetric
  | Sign 
  | CheckSig 
  | GenerateNonce {-| GenerateAsymKeyPair | GenerateSymKey -}
  | Quote
  | PackData | SendRequest | SendResponse | Receive | Certify | Register
                                            
                                            
data ChannelRole = Client | Server
                                            
                                            
data Channel = Channel {
  role :: ChannelRole, 
  channel :: ChannelType
  }

data ChannelType = 
    VChan LibXenVChan 
  | Http HttpInfo
 
data HttpInfo = HttpInfo {
	httpInfoMyServingPort    :: HttpClient.Port,
	httpInfoServingPort      :: HttpClient.Port,
	httpInfoTheirIp 	     :: HttpClient.Hostname,
	httpInfoMaybeConnection  :: (Maybe HttpClient.Connection)
}
                                            
data ArmoredState = ArmoredState {
  knownEntities :: [(Entity, Maybe Channel)],
  executor :: Entity,  --Self
  knownPubKeys :: [(Entity, PublicKey)],
  knownPriKeys :: [(Entity, PrivateKey)]
  }