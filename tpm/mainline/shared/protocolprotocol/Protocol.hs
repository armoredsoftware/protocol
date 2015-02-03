module Protocol where

import Data.Aeson (toJSON, parseJSON, ToJSON,FromJSON, object , (.=), (.:) )
import Demo3Shared hiding (Result)
import Control.Monad
import Control.Monad.State.Strict
import Data.ByteString.Lazy (ByteString, pack, append, empty, cons, fromStrict, length)
import TPM
import qualified Network.Http.Client as HttpClient
import qualified AttesterCAComm as HttpComm
import Demo3Shared hiding (Result)
import qualified Demo3Shared as Demo3 (Shared (Result))
import VChanUtil
import Control.Concurrent.MVar
import Control.Concurrent.STM.TMVar
import qualified Web.Scotty as Scotty
import ProtoTypes


                          	      
runExecute :: Process -> Entity ->IO (Process, ArmoredState)
runExecute proto entity= do
   let emptyvars = []
   let s0 = ArmoredState emptyvars entity []
   runStateT (execute proto) s0

runExecute' :: Process -> ArmoredState ->IO (Process, ArmoredState)
runExecute' proto s0 = do
   runStateT (execute proto) s0

execute :: Process -> ArmoredStateTMonad Process
       --variable, entity, entity, commMethod, followingProc
--	     | CreateChannel Armored Armored Armored Armored Process
execute (CreateChannel achan ent1 ent2 proc) = do
   achan' <- subIfVar achan
   ent1' <- subIfVar ent1
   ent2' <- subIfVar ent2
   
       
           --addChannel achan' (Channel entity1 entity2 (HttpInfo ip2 port2 Nothing )) -- conn))
   execute proc                          
execute (Send mess chan proc) = do
  
  chan' <- subIfVar chan
  case chan' of
   (AChannel str) -> do
     {-s <- get
     let mvarchan = lookup str (channelpairs s)
     case mvarchan of 
      Nothing     -> return (Stuck "attempted to send on channel that is not in state")
      (Just mvar) -> do
       channel <- liftIO $ takeMVar mvar
       case (channel) of
        (Channel _ _ (VchanInfo sendChan _))          -> do 
        	     liftIO $ sendShared' sendChan (armoredToShared mess)
        	     execute proc
        (Channel _ _ (HttpInfo ip1 port1 connection)) -> do                           
        --newConn <- liftIO $ HttpClient.openConnection theIP thePort
        --theConn <- liftIO $ HttpComm.sendHttp (armoredToShared mess) theIP thePort 
          liftIO $ HttpComm.sendHttp' (armoredToShared mess) connection
          execute proc
        _ -> return (Stuck "error in send attempt. Not a channel possibly?")
       liftIO $ putMVar mvar channel 
       -}
     execute proc 
   _		      -> return (Stuck "attempt to send on non-channel")
  
execute (Receive var chan proc) = do
  chan' <- subIfVar chan
{-  case chan' of
    (AChannel str) -> do
      s <- get
      let mvarchan = lookup str (channelpairs s)
      case mvarchan of 
       Nothing     -> return (Stuck "attempted to receive on channel that is not in state")
       (Just mvar) -> do
         channel <- liftIO $ takeMVar mvar
         case channel of
          (Channel _ _ (VchanInfo _ receiveChan)) -> do
                       eitherShared <- liftIO $ receiveShared receiveChan 
                       case (eitherShared) of
                         (Left err)     -> return (Stuck ("ERROR receiving on VChan: " ++ err))
                         (Right shared) -> do 
                                            addVariable var (sharedToArmored shared)
                                            liftIO $ putMVar mvar channel
                                            execute proc  
          (Channel _ _ (HttpInfo ip1 port1 connection)) -> do 
          	       eitherShared <- liftIO $ HttpComm.receiveHttp connection
          	       case eitherShared of
          	         (Left err)     -> return (Stuck err)
          	         (Right shared) -> do
          	         		    addVariable var (sharedToArmored shared)
          	         		    liftIO $ putMVar mvar channel
          	         		    execute proc                                
  --put (ArmoredState ((var,message):(vars s)))
execute (Let var val proc) = do
  s <- get
  --put (ArmoredState ((var,val):(vars s)))
  
  -}
  execute proc
execute (Result res) = return (Result res)
execute (Stop) = return Stop
     
addVariable :: Armored -> Armored -> ArmoredStateTMonad ()
addVariable var val = do
		    s <- get
		    let variables = vars s
		    let variables' = (var,val) : variables
		    --put (ArmoredState (vars s) (executor s) (knownentities s) (channelpairs s))
		    return ()
       --channel name
addChannel :: Armored -> Channel -> ArmoredStateTMonad ()
addChannel (AChannel str) chan = do
			s <- get
	--		let chanls = channelpairs s
	--		chanMVar <- liftIO $ newMVar chan
	--		let chanls' = ( (str,chanMVar) : chanls)
	--		put (ArmoredState (vars s) (executor s) (knownentities s) chanls')
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
	      	     

 

