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
import Control.Concurrent (ThreadId)

data FormalRequest = FormalRequest Entity NRequest deriving (Show)

data NRequest = ProtoNum Int
              | Process Process
              | RequestItem Item Property
              | ReqLS [NRequest]
              | TierRequest [NRequest] deriving ( Show)
data NResponse = No
               | Measurement Item Property Value
               | Counter [(Item,[Property])] NRequest
               | RespLS [NResponse] deriving (Show)
--Privacy Policy related types -----------------------------------------------------------------------
data Item = OS
          | VC
          | PCR [Int]
          | ID deriving (Eq, Show)

data Property = Name
              | Version deriving (Eq, Show)
 
type PrivacyPolicy = [PrivacyRule]
data PrivacyRule = Reveal [(Item,[Property])] Condition deriving (Eq, Show)
data Condition = Equals Item Property Value
               | OneOf Item Property  [Value]
               | NoneOf Item Property [Value]
               | GTV Item Property Value
               | LTV Item Property Value
               | GTETV Item Property Value
               | LTETV Item Property Value
               | Or Condition Condition
               | And Condition Condition deriving (Eq, Show)


data Value = ValString String
           | ValInt Int
           | ValDouble Double
           | ValBool Bool
           | ValByteString ByteString deriving (Eq, Show)

data ArmoredConfig = ArmoredConfig NRequest Condition PrivacyPolicy deriving ( Show)
--end Privacy Policy related types -------------------------------------------------------------------
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
	     | Stop deriving (Show)

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
             | Target
	     | AAppraiser
	     | AMeasurer
	     | APrivacyCA
	     | AFailure String deriving (Show)
data ChannelEntry = ChannelEntry {
		channelEntryName    :: String,
		channelEntryChannel :: Channel		
		} deriving (Show)

instance Eq ChannelEntry where
  (ChannelEntry name1 chan1) == (ChannelEntry name2 chan2) = chan1 == chan2
data Channel = Channel {
	channelEntity      :: Entity,
	channelInfo :: ChannelInfo
	} deriving (Show)


--di this because the deriving Eq ends thinks when I change the name, it's a new channel, not so!
instance Eq Channel where
  (Channel ent1 info1) == (Channel ent2 info2) = ent1 == ent2 && info1 == info2
                           -- server chan         client channel
data ChannelInfo = VChanInfo {
		    vChanInfoMaybeChan    :: (Maybe LibXenVChan)
		 	}
		 | HttpInfo{
                    httpInfoThreadID         :: Maybe ThreadId,
		    httpInfoMyServingPort    :: HttpClient.Port,
		    httpInfoTheirServingPort :: Maybe HttpClient.Port,
		    httpInfoTheirIp 	     :: HttpClient.Hostname,
		    httpInfoMaybeConnection  :: (Maybe HttpClient.Connection),
		    httpInfoTMVarMsgList     :: TMVar [Armored],
		    httpInfoTMVarUnit        :: TMVar ()
		   }
	   
instance Show ChannelInfo where
  show (VChanInfo vchan) = ("VChanInfo " ++ (show vchan))
  show (HttpInfo mt mp tp tip mconn tmls tmunit) = ("HttpInfo " ++ (show mp) ++ " " ++ (show tp) ++ " " ++ (show tip))
  
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
 (VChanInfo _) == (HttpInfo _ _ _ _ _ _ _ ) = False
 (VChanInfo m1) == (VChanInfo m1' ) = m1 == m1'
 (HttpInfo mt mp tp tip mconn tmls tmunit) == (HttpInfo mt' mp' tp' tip' mconn' tmls' tmunit') = and [mt == mt', mp == mp', tp == tp', tip == tip']
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
                            getInternalStateMVar       :: MVar InternalState,
                            channelEntriesTMVar :: TMVar [ChannelEntry]                         
                          }                                                     

data InternalState = AppState {
                       appStateTarget :: Entity
                     }
                       
app = Entity {
	        entityName = "Appraiser",
	        entityIp   = (Just "10.100.0.225"),
	        entityId   = Just 3,
	        entityRole = Appraiser,
	        entityNote = (Just "Just a lonely Appraiser")
	      }
	      
att = Entity {
	        entityName = "Attester",
	        entityIp   = (Just "10.100.0.229"),
	        entityId   = Nothing, --Just 2,
	        entityRole = Attester,
	        entityNote = (Just "Just an attestered here to do your bidding")
	      }

pCA = Entity {
	        entityName = "PrivacyCA",
	        entityIp   = (Just "10.100.0.6"),
	        entityId   = Nothing,
	        entityRole = PrivacyCA,
	        entityNote = (Just "Just a lonely Privacy CA out here in the deep web")
	      }	
instance ToJSON FormalRequest where
       toJSON (FormalRequest ent nreq) = object
                   [ "Entity" .= toJSON ent
                   , "NRequest" .= toJSON nreq
                   ]
instance FromJSON FormalRequest where
       parseJSON (DA.Object o) = FormalRequest <$> o .: "Entity"
                                               <*> o .: "NRequest"
instance ToJSON NRequest where
       toJSON (ProtoNum i) = object
                   [ "ProtoNum" .= DA.String "NRequest"
                   , "Num"      .= i
                   ]
       toJSON (Process proc)  = object
                   [ "Process" .= DA.String "NRequest"
                   , "Proc"    .= DA.String "TODO"
                   ]
       toJSON (RequestItem item property) = object
                   [ "RequestItem" .= DA.String "NRequest"
                   , "Item"        .= toJSON item
                   , "Property"    .= toJSON property
                   ]
       toJSON (ReqLS ls) = object
                   [ "ReqLS" .= DA.String "NRequest"
                   , "Requests" .= toJSON ls
                   ]
       toJSON (TierRequest ls) = object
                   [ "TierRequest" .= DA.String "NRequest"
                   , "Requests"    .= toJSON ls
                   ]
instance FromJSON NRequest where
         parseJSON (DA.Object o) | HM.member "ProtoNum" o    = ProtoNum    <$> o .: "Num"
                                 -- | HM.member "Process" o     = Process     <$> o .: "Proc" --TODO implement to/fro JSON for Process... :D
                                 | HM.member "RequestItem" o = RequestItem <$> o .: "Item"
                                                                           <*> o .: "Property"
                                 | HM.member "ReqLS" o       = ReqLS       <$> o .: "Requests"
                                 | HM.member "TierRequest" o = TierRequest <$> o .: "Requests"

instance ToJSON NResponse where
         toJSON No = object
                ["No" .= DA.String "NResponse"]
         toJSON (Measurement item prop val) = object
                ["Measurement" .= DA.String "NResponse"
                , "Item"       .= toJSON item
                , "Property"   .= toJSON prop 
                , "Value"      .= toJSON val
                ]
         toJSON (Counter itemProplsLS req) = object
                ["Counter"     .= DA.String "NResonse"
                , "ItemPropertylsLS" .= toJSON itemProplsLS --I really hope this is all I have to do here..
                , "Request"          .= toJSON req 
                ]
         toJSON (RespLS responselS) = object
                ["RespLS"         .= DA.String "NResponse"
                , "Responses"     .= toJSON responselS
                ]
instance FromJSON NResponse where
         parseJSON (DA.Object o) | HM.member "No" o = pure No 
                                 | HM.member "Measurement" o = Measurement <$> o .: "Item"
                                                                           <*> o .: "Property"
                                                                           <*> o .: "Value"
                                 | HM.member "Counter" o = Counter <$> o .: "ItemPropertylsLS"
                                                                   <*> o .: "Request"
                                 | HM.member "RespLS" o = RespLS   <$> o .: "Responses"

instance ToJSON Item where
         toJSON OS = object
                ["OS" .= DA.String "Item"]
         toJSON VC = object
                ["VC" .= DA.String "Item"]
         toJSON (PCR intLS) = object
                [ "PCR" .= DA.String "Item"
                , "Ints" .= toJSON intLS
                ]         
         toJSON ID = object
                ["ID" .= DA.String "Item"]
instance FromJSON Item where
         parseJSON (DA.Object o) | HM.member "OS" o = pure OS
                                 | HM.member "VC" o = pure VC
                                 | HM.member "PCR" o = PCR <$> o .: "Ints"
                                 | HM.member "ID" o = pure ID

instance ToJSON Property where
         toJSON Name = object
                [ "Name" .= DA.String "Property"]
         toJSON Version = object
                [ "Version" .= DA.String "Property"]                
instance FromJSON Property where
         parseJSON (DA.Object o) | HM.member "Name" o = pure Name
                                 | HM.member "Version" o = pure Version

instance ToJSON PrivacyRule where
         toJSON (Reveal itemProplsLS condition) = object
                ["Reveal" .= DA.String "PrivacyRule"
                , "ItemPropertylsLS" .= toJSON itemProplsLS 
                , "Condition"        .= toJSON condition
                ]
instance FromJSON PrivacyRule where
         parseJSON (DA.Object o) | HM.member "Reveal" o = Reveal <$> o .: "ItemPropertylsLS"
                                                                 <*> o .: "Condition"

instance ToJSON Condition where
         toJSON (Equals item prop val) = object
                [ "Equals" .= DA.String "Condition"
                , "Item"   .= toJSON item
                , "Property" .= toJSON prop 
                , "Value"    .= toJSON val
                ]
         toJSON (OneOf item prop valLS) = object
                [ "OneOf" .= DA.String "Condition"
                , "Item"   .= toJSON item
                , "Property" .= toJSON prop
                , "Values"    .= toJSON valLS
                ]  
         toJSON (NoneOf item prop valLS) = object
                [ "NoneOf" .= DA.String "Condition"
                , "Item"   .= toJSON item
                , "Property" .= toJSON prop
                , "Values"    .= toJSON valLS
                ] 
         toJSON (GTV item prop val) = object
                [ "GTV" .= DA.String "Condition"
                , "Item"   .= toJSON item
                , "Property" .= toJSON prop
                , "Value"    .= toJSON val
                ] 
         toJSON (LTV item prop val) = object
                [ "LTV" .= DA.String "Condition"
                , "Item"   .= toJSON item
                , "Property" .= toJSON prop
                , "Value"    .= toJSON val
                ] 
         toJSON (GTETV item prop val) = object
                [ "GTETV" .= DA.String "Condition"
                , "Item"   .= toJSON item
                , "Property" .= toJSON prop
                , "Value"    .= toJSON val
                ] 
         toJSON (LTETV item prop val) = object
                [ "LTETV" .= DA.String "Condition"
                , "Item"   .= toJSON item
                , "Property" .= toJSON prop
                , "Value"    .= toJSON val
                ]
         toJSON (Or c1 c2) = object
                [ "Or" .= DA.String "Condition"
                , "Condition1" .= toJSON c1
                , "Condition2" .= toJSON c2
                ]
         toJSON (And c1 c2) = object
                [ "And" .= DA.String "Condition"
                , "Condition1" .= toJSON c1
                , "Condition2" .= toJSON c2
                ]
instance FromJSON Condition where
         parseJSON (DA.Object o) | HM.member "Equals" o = Equals <$> o .: "Item"
                                                                 <*> o .: "Property"
                                                                 <*> o .: "Value"
                                 | HM.member "OneOf" o = OneOf   <$> o .: "Item"
                                                                 <*> o .:  "Property"
                                                                 <*> o .:  "Values"
                                 | HM.member "NoneOf" o = NoneOf <$> o .: "Item"
                                                                 <*> o .: "Property"
                                                                 <*> o .: "Values"
                                 | HM.member "GTV" o    = GTV    <$> o .: "Item"
                                                                 <*> o .: "Property"
                                                                 <*> o .: "Value"
                                 | HM.member "LTV" o    = LTV    <$> o .: "Item"
                                                                 <*> o .: "Property"
                                                                 <*> o .: "Value"
                                 | HM.member "GTETV" o    = GTETV <$> o  .: "Item"
                                                                 <*> o .: "Property"
                                                                 <*> o .: "Value"
                                 | HM.member "LTETV" o    = LTETV <$> o .: "Item"
                                                                 <*> o .: "Property"
                                                                 <*> o .: "Value"
                                 | HM.member "Or" o = Or <$> o .: "Condition1"
                                                         <*> o .: "Condition2"
                                 | HM.member "And" o = And <$> o .: "Condition1"
                                                           <*> o .: "Condition2"
 
instance ToJSON Value where
         toJSON (ValString str) = object
                ["ValString" .= DA.String "Value"
                , "String"   .= toJSON str
                ]
         toJSON (ValInt int) = object
                ["ValInt" .= DA.String "Value"
                , "Int"   .= toJSON int
                ]
         toJSON (ValDouble doub) = object
                ["ValDouble" .= DA.String "Value"
                , "Double"   .= toJSON doub 
                ]
         toJSON (ValBool bool) = object
                ["ValBool" .= DA.String "Value"
                , "Bool"   .= toJSON bool 
                ]
         toJSON (ValByteString bs) = object
                ["ValByteString" .= DA.String "Value"
                , "ByteString"   .= encodeToText (toStrict bs) 
                ]

instance FromJSON Value where
         parseJSON (DA.Object o) | HM.member "ValString" o = ValString <$> o .: "String"
                                 | HM.member "ValInt" o = ValInt <$> o .: "Int"
                                 | HM.member "ValDouble" o = ValDouble <$> o .: "Double"
                                 | HM.member "ValBool" o = ValBool <$> o .: "Bool"
                                 | HM.member "ValByteString" o = ValByteString <$> ((o .: "ByteString") >>= decodeFromTextL)
         
instance ToJSON ArmoredConfig where
         toJSON (ArmoredConfig req condition pripo) = object
                        [ "ArmoredConfig" .= DA.String "ArmoredConfig"
                        , "Request"       .= toJSON req 
                        , "Condition"     .= toJSON condition
                        , "PrivacyPolicy" .= toJSON pripo 
                        ]
instance FromJSON ArmoredConfig where
         parseJSON (DA.Object o) | HM.member "ArmoredConfig" o = ArmoredConfig <$> o .: "Request"
                                                                               <*> o .: "Condition"
                                                                               <*> o .: "PrivacyPolicy"
                          
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
					                 
