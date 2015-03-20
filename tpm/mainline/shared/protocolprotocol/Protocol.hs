module Protocol where

import Data.Aeson (toJSON, parseJSON, ToJSON,FromJSON, object , (.=), (.:) )
import Demo3Shared hiding (Result)
import Control.Monad
import Control.Monad.State.Strict
import Data.ByteString.Lazy (ByteString, pack, append, empty, cons, fromStrict, length)
import TPM
import qualified Network.Http.Client as HttpClient
import qualified CommTools as CommTools
import Demo3Shared hiding (Result)
import qualified Demo3Shared as Demo3
import VChanUtil
import Control.Concurrent.MVar
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM
import qualified Web.Scotty as Scotty
import ProtoTypes
import CommunicationNegotiator
import CommTools hiding (Result)
import Control.Concurrent
import Data.Tuple
import ExampleArmoredConfig
                          	      
runExecute :: Process -> Entity ->IO (Process, ArmoredState)
runExecute proto entity= do
   let emptyvars = []
   e <- newTMVarIO []
   let s0 = ArmoredState emptyvars entity [] e
   forkIO $ do 
   	     runStateT negotiator s0
   	     return ()
   runStateT (execute proto) s0

runExecute' :: Process -> ArmoredState ->IO (Process, ArmoredState)
runExecute' proto s0 = do
   runStateT (execute proto) s0

execute :: Process -> ArmoredStateTMonad Process
       --variable, entity, entity, commMethod, followingProc
--	     | CreateChannel Armored Armored Armored Armored Process
execute (Let var val proc) = do 
   --var' <- subIfVar var
   val' <- subIfVar val
   addVariable var val'
   liftIO $ putStrLn ("performing let: variable: " ++ (show var) ++ (" val: " ++ (show val')))
   execute proc
   
execute (CreateChannel achan ent1 proc) = do
   liftIO $ putStrLn "EXECUTING CREATECHANNEL COMMAND\n"
   achan' <- subIfVar achan
   ent1' <- subIfVar ent1
   case achan' of
     (AChannel chanName) ->
       case ent1' of
        (AEntity ent1'') -> do
          mVChannel <- maybeCreateVChannelWith ent1'' chanName
          case mVChannel of 
            Nothing -> do
              let str = "vchan failed. Here is where Http channel set up should be."
              liftIO $ putStrLn $ str
              mHttpChannel <- tryCreateHttpChannel ent1'' chanName 
              case mHttpChannel of
                Nothing -> do
                  let  str2 = "super error! no channel created. vchan and http failed."                 
                  liftIO $ putStrLn str2
                  return $ Stuck str2
                Just hChan -> do
                  let str3 = "successfully created httpChannel"
                  liftIO $ putStrLn $ str3
                  --http chan added to state in tryCreateHttpChannel
                  execute proc 
            Just vchan -> do --could be because channel existed, or because I just made it. 
              execute proc  
        (_)              -> do
          liftIO $ putStrLn "not an entity in create channel!!! stopping now."
          return $ Stuck "didn't have an entity in the CreateChannel command. sorry, I gave up."
     a@_ -> do 
       let err = "unexpected type in first argument to CreateChannel: " ++ (show a) ++ " stuck!"
       liftIO $ putStrLn $ err
       return $ Stuck err
   
       
           --addChannel achan' (Channel entity1 entity2 (HttpInfo ip2 port2 Nothing )) -- conn))                        
execute (Send mess chan proc) = do
  liftIO $ putStrLn "sending on channel"
  chan' <- subIfVar chan
  mess' <- subIfVar mess 
  case chan' of
   (AChannel str) -> do
     s <- get
     let chanEntryTMVar = channelEntriesTMVar s 
     chanEntryLS <- liftIO $ atomically $ takeTMVar chanEntryTMVar 
     case lookupViaName str chanEntryLS of
       Nothing        -> do 
         liftIO $ putStrLn $ "Error for now: Send without creating first. easily change to create channel call here."
         liftIO $ atomically $ putTMVar chanEntryTMVar chanEntryLS
       Just chanEntry -> do 
         let chan = channelEntryChannel chanEntry 
         liftIO $ putStrLn $ "About to send: " ++ (show mess') ++ " which converted to shared is: " ++ (show (armoredToShared mess'))
         liftIO $ atomically $ putTMVar chanEntryTMVar chanEntryLS
         liftIO $ sendG chan mess' 
     execute proc 
   _		      -> return (Stuck "attempt to send on non-channel")
  
execute (Receive var chan proc) = do
  liftIO $ putStrLn "receiving on channel"
  chan' <- subIfVar chan
  case chan' of
   (AChannel str) -> do
     s <- get
     let chanEntryTMVar = channelEntriesTMVar s 
     chanEntryLS <- liftIO $ atomically $ takeTMVar chanEntryTMVar 
     --liftIO $ putStrLn $ "looking for channel named: " ++ str ++ " in " ++ (show chanEntryLS)
     case lookupViaName str chanEntryLS of
       Nothing        -> do 
         let strer = "Error!! channel with name: " ++ str ++ " not found!!"
         liftIO $ putStrLn strer 
         liftIO $ atomically $ putTMVar chanEntryTMVar chanEntryLS
         return (Stuck str)
       Just chanEntry -> do 
         let chan = channelEntryChannel chanEntry 
         armoredGift <- liftIO $ receiveG chan
         liftIO $ atomically $ putTMVar chanEntryTMVar chanEntryLS
         case armoredGift of
           AFailure str -> do 
             liftIO $ putStrLn ("Crap. failed in recieved: " ++ str )
             return $ Stuck str 
           x@_          -> do 
             addVariable var x 
             execute proc 
   _		      -> return (Stuck "attempt to send on non-channel")
execute (Result res) = do
			res' <- subIfVar res
			liftIO $ putStrLn ("Result: " ++ (show res'))			
			return (Result res')
execute (Stop) = return Stop
     
addVariable :: Armored -> Armored -> ArmoredStateTMonad ()
addVariable var val = do
		    s <- get
		    let variables = vars s
		    let variables' = (var,val) : variables
		    put (ArmoredState variables' (executor s) (knownentities s) (channelEntriesTMVar s))
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

lookupViaName :: String  -> [ChannelEntry] -> Maybe ChannelEntry
lookupViaName str [] = Nothing
lookupViaName str (x:xs) = case str == (channelEntryName x) of
                             True  -> Just x 
                             False -> lookupViaName str xs 

lookupViaChan :: Channel -> [ChannelEntry] -> Maybe ChannelEntry
lookupViaChan chan [] = Nothing
lookupViaChan chan (x:xs) = case chan == (channelEntryChannel x) of
                             True  -> Just x 
                             False -> lookupViaChan chan xs 

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
	      	     

 

