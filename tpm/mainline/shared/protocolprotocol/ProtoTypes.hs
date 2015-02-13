{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, OverloadedStrings, RecordWildCards  #-}
module ProtoTypes where

import Data.Aeson (toJSON, parseJSON, ToJSON,FromJSON, object , (.=), (.:) )
import Demo3Shared hiding (Result)
import Control.Monad
import Control.Monad.State.Strict
import Data.ByteString.Lazy (ByteString, pack, append, empty, cons, fromStrict, length)
import TPM
import qualified Network.Http.Client as HttpClient
--import qualified Demo3Shared as Demo3
import VChanUtil
import Control.Concurrent.MVar
import Control.Concurrent.STM.TMVar
import Web.Scotty
import qualified Data.Aeson as DA (Value(..), encode, decode, eitherDecode) --for JSON stuff
import Control.Applicative ( (<$>), (<*>), pure )                           --for JSON stuff
import qualified Data.HashMap.Strict as HM (member, lookup)                 --for JSON stuff
import Data.Aeson (toJSON, parseJSON, ToJSON,FromJSON, object , (.=), (.:) )--for JSON stuff
import Data.ByteString.Lazy(ByteString, empty, append, pack, toStrict, fromStrict) --for JSON stuff

-- these are the 'verbs'
data Process = Send Armored Armored Process
	         --  variable mess, channel 
	     | Receive Armored Armored Process  --first is variable
	     	 -- variable mess, variable new chan 
	     | ReceiveAny Armored Armored Process-- the last armored is a variable name for the new channel
	                 --chanName, entity, entity, followingProc
	     | CreateChannel Armored Armored Process
	         --var,    val,    next proc
	     | Let Armored Armored Process
	            --   _  with  _  store var  nextProc  
	     | Encrypt Armored Armored Armored Process
	                   -- _ with  _  store _, success,  failure
	     | CaseDecrypt Armored Armored Armored Process Process
	           --var     val1   proc1,  val2    proc2
	     | Case Armored Armored Process Armored Process
	                --sig    key   succeed  fail
	     | CheckSig Armored Armored Process Process
	              --array  do      finally
	     | ForEach Armored Process Process
	                 --array   val     finally
	     | AppendArray Armored Armored Process--first armored is array, second is val to append
	     | Result Armored
	     | Stuck String
	     | Stop

--these are the 'nouns'	   
--putOnArmor :: a -> Armored
--putOnArmor ..  
data Armored = Var String
	     | ARequest Request
 	     | AResponse Response
             | AEvidenceDescriptor EvidenceDescriptor
	     | AEvidencePiece EvidencePiece
	     | ACARequest CARequest
	     | ACAResponse CAResponse 
	     | APair Armored Armored 
--	     | AKey Key  --because show complains commented out.
	     | AEncrypted ByteString
	     | AEntity Entity 
	     | AChannel String
	     | ATPM_PCR_SELECTION TPM_PCR_SELECTION
	     | ATPM_NONCE TPM_NONCE 
	     | ArmoredPCRSel [Int]
	     | ArmoredCreateNonce
	     | ArmoredCreateDesiredEvidence [Int]
	     | ArmoredRequesetForAttest Armored Armored Armored
	     | ArmoredCreateChannelWith Armored
	     | AMyself --entity stored in state that is the executor
	     | ArmoredEvaluate Armored Armored
	     | ArmoredCreateCACertReq Armored
	     | ArmoredCAChannel
	     | ArmoredEmptyArray
	     | Array String
	     | ArmoredExtractDesiredEvidence
	     | ArmoredArrayItem 
	     | ArmoredMeasurerChan
	     | ArmoredCreateQuote
	     | ArmoredCreateAppResponse
	     | AAttester  --constants for state
	     | AAppraiser
	     | AMeasurer
	     | APrivacyCA
	     | AFailure String deriving (Show)
data ChannelEntry = ChannelEntry {
		channelEntryName    :: String,
		channelEntryChannel :: Channel		
		} 
data Channel = Channel {
	channelEntity      :: Entity,
	channelInfo :: ChannelInfo
	} deriving (Eq, Show)
                           -- server chan         client channel
data ChannelInfo = VChanInfo {
		    vChanInfoMaybeSendChan    :: (Maybe LibXenVChan),
		    vChanInfoMaybeReceiveChan :: (Maybe LibXenVChan)
		 	}
		 | HttpInfo{
		    httpInfoMyServingPort    :: HttpClient.Port,
		    httpInfoTheirServingPort :: HttpClient.Port,
		    httpInfoTheirIp 	     :: HttpClient.Hostname,
		    httpInfoMaybeConnection  :: (Maybe HttpClient.Connection),
		    httpInfoTMVarMsgList     :: TMVar [Armored],
		    httpInfoTMVarUnit        :: TMVar ()
		   }
		   
instance Show ChannelInfo where
  show (VChanInfo msend mrec) = ("VChanInfo " ++ (show msend) ++ " " ++ (show mrec))
  show (HttpInfo mp tp tip mconn tmls tmunit) = ("HttpInfo " ++ (show mp) ++ " " ++ (show tp) ++ " " ++ (show tip))
  
data CommRequest = PortRequest {
		   portRequestEntity  :: Entity,		 
		   portRequestPort    :: HttpClient.Port,
		   portRequestNonce   :: Integer
		   }
		 | VChanRequest {
		     vChanRequestEntity :: Entity,
		     vChanReqquestNonce :: Integer		     
		   }

instance Eq ChannelInfo where
 (VChanInfo _ _) == (HttpInfo _ _ _ _ _ _ ) = False
 (VChanInfo m1 m2) == (VChanInfo m1' m2') = and [m1 == m1', m2 == m2']
 (HttpInfo mp tp tip mconn tmls tmunit) == (HttpInfo mp' tp' tip' mconn' tmls' tmunit') = and [mp == mp', tp == tp', tip == tip']
data Role = Appraiser
    	  | Attester
	  | Measurer
          | PrivacyCA deriving ( Eq, Show)


            
data Entity = Entity {
	        entityName    :: String,
	        entityIp   :: Maybe HttpClient.Hostname,	   
	        entityId   :: Maybe Int,
	        entityRole :: Role,
	        entityNote :: Maybe String
	      }
	     deriving (Eq,  Show)	    
	     
data Key = Rsa ByteString
	 | Tpm ByteString
	 -- | etc	     		


type ArmoredStateTMonad a = StateT ArmoredState IO a 
type VariableBindings = [(Armored,Armored)]
data ArmoredState = ArmoredState {
                            vars :: VariableBindings,
                            executor :: Entity,
                            knownentities  :: [Entity],
                            channelEntriesTMVar :: TMVar [ChannelEntry]                         
                          } 
                          
                          
	


app = Entity {
	        entityName = "Appraiser",
	        entityIp   = (Just "10.100.0.246"),
	        entityId   = Just 1,
	        entityRole = Appraiser,
	        entityNote = (Just "Just a lonely Appraiser")
	      }
	      
attAddress = Entity {
	        entityName = "Attester",
	        entityIp   = (Just "10.100.0.208"),
	        entityId   = Just 2,
	        entityRole = Attester,
	        entityNote = (Just "Just an attestered here to do your bidding")
	      }

pCAAddress = Entity {
	        entityName = "PrivacyCA",
	        entityIp   = (Just "10.100.0.6"),
	        entityId   = Nothing,
	        entityRole = PrivacyCA,
	        entityNote = (Just "Just a lonely Privacy CA out here in the deep web")
	      }	



instance ToJSON CommRequest where
	toJSON (PortRequest entity port nonce) =  object 
		   ["PortRequest" .= DA.String "CommRequest"
		   , "Entity" .= toJSON entity				    
		   , "Port"   .= port
		   , "Nonce"  .= nonce
		   ]
	toJSON (VChanRequest entity nonce) = object [ "VChanRequest" .= DA.String "CommRequest" 
					      , "Entity"       .= toJSON entity
					      , "Nonce"	       .= nonce
					      ]						  
instance FromJSON CommRequest where
	parseJSON (DA.Object o) | HM.member "PortRequest" o = PortRequest <$> o .: "Entity"
									  <*> o .: "Port"
									  <*> o .: "Nonce"
				| HM.member "VChanRequest" o = VChanRequest <$> o .: "Entity"
									    <*> o .: "Nonce"
instance ToJSON Entity where
	toJSON (Entity name mip mid role mnote) = object [ "EntityName" .= name
						         , "EntityIp"   .= toJSON mip
						         , "EntityId"   .= mid
						         , "EntityRole" .= toJSON role
						         , "EntityNote" .= mnote
						         ]
instance FromJSON Entity where
	parseJSON (DA.Object o) = Entity <$> o .: "EntityName"
					 <*> o .: "EntityIp"
					 <*> o .: "EntityId"
					 <*> o .: "EntityRole"
					 <*> o .: "EntityNote"
					 
instance ToJSON Role where
	toJSON Appraiser = DA.String "Appraiser"
	toJSON Attester  = DA.String "Attester"
	toJSON Measurer  = DA.String "Measurer"
        toJSON PrivacyCA = DA.String "PrivacyCA"				 
        
instance FromJSON Role where
	parseJSON (DA.String "Appraiser") = pure Appraiser
	parseJSON (DA.String "Attester")  = pure Attester
	parseJSON (DA.String "Measurer")  = pure Measurer
	parseJSON (DA.String "PrivacyCA") = pure PrivacyCA        
					                 
