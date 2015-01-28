module Protocol where

import Data.Aeson (toJSON, parseJSON, ToJSON,FromJSON, object , (.=), (.:) )
import Demo3Shared hiding (Result)
import Control.Monad
import Control.Monad.State.Strict
import Data.ByteString.Lazy (ByteString, pack, append, empty, cons, fromStrict, length)
import TPM

import Demo3Shared hiding (Result)
import VChanUtil
-- these are the 'verbs'
data Process = Send Armored Armored Process
	         --  variable mess, channel 
	     | Receive Armored Armored Process  --first is variable
	     	 -- variable mess, variable new chan 
	     | ReceiveAny Armored Armored Process-- the last armored is a variable name for the new channel
	                 --variable, entity, entity, commMethod, followingProc
	     | CreateChannel Armored Armored Armored Armored Process
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
	     | AChannel Channel
	     | ATPM_PCR_SELECTION TPM_PCR_SELECTION
	     | ATPM_NONCE TPM_NONCE 
	     | ArmoredPCRSel [Int]
	     | ArmoredCreateNonce
	     | ArmoredCreateDesiredEvidence [Int]
	     | ArmoredRequesetForAttest Armored Armored Armored
	     | ArmoredCreateChannel Armored
	     | AMyself --entity stored in state that is the executor
	     | ArmoredEvaluate Armored Armored
	     | ArmoredCreateCACertReq Armored
	     | ArmoredCAChannel
	     | ACommMethod CommMethod
	     | ArmoredEmptyArray
	     | Array String
	     | ArmoredExtractDesiredEvidence
	     | ArmoredArrayItem 
	     | ArmoredMeasurerChan
	     | ArmoredCreateQuote
	     | ArmoredCreateAppResponse
	     | AAttester  --constants from state
	     | AAppraiser
	     | AMeasurer
	     | APrivacyCA
	     | AFailure String deriving (Show)
	     
data Channel = Channel Entity Entity ChannelInfo deriving (Eq, Show)
                           -- server chan         client channel
data ChannelInfo = VchanInfo (Maybe LibXenVChan) (Maybe LibXenVChan)
		 | HttpInfo (Maybe (String, String)) (Maybe Connection)
		 deriving (Eq, Show)
data Connection = Connection deriving (Eq, Show) --TODO not really, this is a scotty thing.     	     
data Entity = Appraiser Address
	    | Attester Address
	    | Measurer Address
            | PrivacyCA Address deriving ( Eq, Show)

getAddress :: Entity -> Address
getAddress (Appraiser a) = a
getAddress (Attester a)  = a
getAddress (Measurer a) = a
getAddress (PrivacyCA a) = a
            
data Address = Address {
	        name :: String,
	        ip   :: Maybe String,
	        port :: Maybe String,
	        getid   :: Maybe Int,
	        note :: Maybe String
	      }
	     deriving (Eq,  Show)	    
	     
data Key = Rsa ByteString
	 | Tpm ByteString
	 -- | etc	     
data CommMethod = VChan
		| Http deriving (Eq,  Show)		


type ArmoredStateTMonad a = StateT ArmoredState IO a 
type VariableBindings = [(Armored,Armored)]
data ArmoredState = ArmoredState {
                            vars :: VariableBindings,
                            executor :: Entity,
                            knownentities :: [Entity]
                            
--                            channelpairs :: [(Channel,ChannelInfo)]
                          } deriving ( Show)
                          	      
runExecute :: Process -> Entity ->IO (Process, ArmoredState)
runExecute proto entity= do
   let emptyvars = []
   let s0 = ArmoredState emptyvars entity []
   runStateT (execute proto) s0

execute :: Process -> ArmoredStateTMonad Process
       --variable, entity, entity, commMethod, followingProc
--	     | CreateChannel Armored Armored Armored Armored Process
execute (CreateChannel var ent1 ent2 commMethod proc) = do
   ent1' <- subIfVar ent1
   ent2' <- subIfVar ent2
   commMethod' <- subIfVar commMethod
   case commMethod' of
     (ACommMethod VChan) -> do
       let maybeID = (case (ent1',ent2') of
        		(AMyself, (AEntity e)) -> ( getid $ getAddress e)
        		((AEntity e), AMyself) -> ( getid $ getAddress e ))
       case maybeID of
        Nothing -> return (Stuck "Either ID not given or attempted comm chan without include self. Currently not supported")
        (Just i) -> do
          sendChan <-liftIO $ server_init i
          receiveChan <- liftIO $ client_init i
          let (AEntity entity1) = ent1'
          let (AEntity entity2) = ent2'
          addVariable var (AChannel (Channel entity1 entity2 (VchanInfo (Just sendChan) (Just receiveChan))))
          execute proc
     (ACommMethod Http) -> do
       let maybeIPPort = (case (ent1',ent2') of
        	(AMyself, (AEntity e)) -> (case (ip (getAddress e), port (getAddress e)) of
                         (Nothing, _) -> Nothing
                         (_, Nothing) -> Nothing
                         (Just j1, Just j2) -> (Just (j1,j2)))
                ((AEntity e), AMyself) -> (case (ip (getAddress e), port (getAddress e)) of
                         (Nothing, _) -> Nothing
                         (_, Nothing) -> Nothing
                         (Just j1, Just j2) -> (Just (j1,j2))) )
       case maybeIPPort of
         Nothing -> return (Stuck "IP or port not given or attempted to make channel without self. currently not supported.")
         (Just ipportpair) -> do
           let (AEntity entity1) = ent1'
           let (AEntity entity2) = ent2'
           addVariable var (AChannel (Channel entity1 entity2 (HttpInfo (Just ipportpair) Nothing)))
           execute proc                          
execute (Send mess chan proc) = do
  s <- get
  execute proc
execute (Receive var chan proc) = do
  --message <- receive  --TODO
  s <- get
  --put (ArmoredState ((var,message):(vars s)))
  execute proc
execute (Let var val proc) = do
  s <- get
  --put (ArmoredState ((var,val):(vars s)))
  execute proc
execute (Result res) = return (Result res)
execute (Stop) = return Stop
     
addVariable :: Armored -> Armored -> ArmoredStateTMonad ()
addVariable var val = do
		    s <- get
		    let variables = vars s
		    let variables' = (var,val) : variables
		    put (ArmoredState (vars s) (executor s) (knownentities s))
		    return ()
		    
typeCheckProcess :: Process -> Either String Bool
typeCheckProcess proc = Left "fail" --TODO      	      


subIfVar ::  Armored -> ArmoredStateTMonad  Armored
subIfVar  armItem = do
   s <- get
   return $ subIfVar'  armItem (vars s) 

subIfVar' ::  Armored -> VariableBindings ->  Armored 
subIfVar'  armItem gamma = case myLookup  armItem gamma of
   Nothing ->  armItem
   Just val -> val
                    
myLookup ::  Armored -> VariableBindings -> Maybe  Armored
myLookup  armItem []     = Nothing
myLookup  arm@(Var str) (((Var str2),val):xs) = if str == str2 
				    then Just val
				    else myLookup arm xs
myLookup any@(_) (x:xs)  = myLookup any xs        				                                                   
	      	     
	     
