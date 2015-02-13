{-# LANGUAGE OverloadedStrings #-}

module CommunicationNegotiator where 


import Web.Scotty hiding (get, put)
import qualified Web.Scotty as Scotty
import qualified Demo3Shared as AD
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S
import qualified Data.Text.Lazy as LazyText
import Network.Http.Client hiding (get)
import qualified Data.Text.Lazy.Encoding as LazyEncoding
import Control.Concurrent.MVar
import Control.Concurrent.STM.TMVar
import qualified Network.Http.Client as HttpClient
import Data.ByteString.Lazy (ByteString, pack, append, empty, cons, fromStrict, length)
--import Control.Monad
import Control.Monad.State.Strict
import ProtoTypes
import Demo3Shared
import CommTools
import Control.Monad.STM
import System.Random
import VChanUtil
import Data.Time
import Control.Concurrent
negotiationport = 3000


                 
 
 {-
requestPort :: HttpClient.Hostname -> IO (Either String HttpClient.Port)
requestPort ip = do
		  chan <- sendHttp (WPort 2999) ip negotiationport
		  eitherShared <- receiveHttp chan
		  case eitherShared of
		   (Left err)           -> return (Left err)
		   (Right (WPort port)) -> return (Right port)
		   (Right _)             -> return (Left "that's weird.. received something that's not a port..")
-}

channelExistsWith :: [ChannelEntry] -> Entity -> IO (Maybe Channel)
channelExistsWith [] e     = return Nothing
channelExistsWith (x:xs) e = do
			  let c = channelEntryChannel x
			  let ent = channelEntity c
			  if ent == e then return (Just c)
			  	      else channelExistsWith xs e
			  	      
			    
vChanSendAndListen :: LibXenVChan -> LibXenVChan -> Shared -> (MVar (Either String Shared)) -> IO ()
vChanSendAndListen sendChan receiveChan message mvar = do
  sendShared' sendChan message
  eitherShared <- receiveShared receiveChan
  putMVar mvar eitherShared
   	
vChanMVarListener :: LibXenVChan -> MVar (Either String Shared) -> IO ()
vChanMVarListener rChan mvar = do
				eitherShared <- receiveShared rChan
				putMVar mvar eitherShared
				return ()
				  			    
pingVChannel :: LibXenVChan -> LibXenVChan -> Integer -> Integer -> IO Bool
pingVChannel chan receiveChan n1 n2= do
		     respMVar <- newEmptyMVar :: IO (MVar (Either String Shared))
		     forkIO $ vChanSendAndListen chan receiveChan (WNonce (n1 + 1)) respMVar		      
		     curTime <- getCurrentTime
		     let seconds = utctDayTime curTime
		     waitPatiently respMVar seconds
		     --mvar should have been filled with something at this point
		     eitherStringShared <- takeMVar respMVar
		     case eitherStringShared of
		     	(Left err) -> do
		     			putStrLn err
		     			return False
		     	(Right (WNonce shouldBeN2plus1)) -> do
		     				if shouldBeN2plus1 == (n2 +1)
		     				  then return True
		     				  else do
		     				   putStrLn "Wronge respNONCE"
		     				   return False
			(Right _)	   -> do
						putStrLn "Received something weird instead of nonce response"
						return False

vChanTimeout = 1
waitPatiently :: (MVar (Either String Shared)) -> DiffTime -> IO ()
waitPatiently mvar startSeconds = do
  			isEmpt <- isEmptyMVar mvar
  			case isEmpt of
  			  False -> return () --something is there! we are done!
  			  True  -> do
				curTime <- getCurrentTime
				let curSeconds = utctDayTime curTime
				if (curSeconds - startSeconds) > vChanTimeout 
				  then do
				  	isEmpt <- isEmptyMVar mvar --check again if it's empty
				  	if isEmpt then putMVar mvar (Left "Timeout") -- if still empty,
				  		  else return ()		     -- if not empty, yay! 
				  	
				  else do -- we haven't timed out yet.
				  	yield 
				  	waitPatiently mvar startSeconds
		     	    
attemptVChanContactR :: Int ->Integer -> Integer-> IO (Maybe (LibXenVChan,LibXenVChan))
attemptVChanContactR id n1 n2 = do
			   receiveChan <- server_init id
			   maybeSendChan <-  maybe_client_init id --client_init' id False
			   --remember, to get to this point means the other guy sent a request which means he is listening on his little vchannel
			   case maybeSendChan of
			     (Nothing)   -> return Nothing --then we def know not local guy
			     (Just chan) -> do
			     	 
			         success <- pingVChannel chan receiveChan n1 n2
			         case success of
			           False -> return Nothing
			           True ->  return (Just (chan,receiveChan))			         

maybeCreateChannelWith :: Entity -> ArmoredStateTMonad (Maybe Channel)
maybeCreateChannelWith ent = do
  s <- get
  let chanETMVar = channelEntriesTMVar s
  let me = executor s
  chanls <- liftIO $ atomically $ takeTMVar chanETMVar
  chanExists <- liftIO $ channelExistsWith chanls ent
  case chanExists of
    (Just channel) -> do
    			liftIO $ atomically $ putTMVar chanETMVar chanls
    			return (Just channel)
    (Nothing)      -> do
    			mVChannel <- liftIO $ tryCreateVChannel me ent
    			liftIO $ atomically $ putTMVar chanETMVar chanls
    		        return mVChannel
eitherReceiveSharedWTimeout :: LibXenVChan -> IO (Either String Shared)
eitherReceiveSharedWTimeout rchan = do
				mvar <- newEmptyMVar :: IO (MVar (Either String Shared))
				forkIO $ vChanMVarListener rchan mvar
				curTime <- getCurrentTime
		     		let seconds = utctDayTime curTime
		     		waitPatiently mvar seconds
		     		eitherStringShared <- takeMVar mvar --at this point guarenteed to not be empty
		     		case eitherStringShared of
		     		 (Left err)  -> do
		     		 		 putStrLn err
		     		 		 return (Left err)
		     		 (Right shared) -> return (Right shared)
		     		 
		     		
				      			        
tryCreateVChannel :: Entity -> Entity -> IO (Maybe Channel)
tryCreateVChannel me ent = do
			 let mID = entityId ent
			 case mID of
			   (Nothing) -> return Nothing -- entity has no id, can't use vChan!
			   (Just id) -> do
			   		  rChan <- server_init id
			   		  let mIP = entityIp ent
			   		  case mIP of
			   		   Nothing -> return Nothing
			   		   (Just ip) -> do 
			   		      conn <-liftIO $ openConnection ip negotiationport
			   		      n1 <- randomIO :: IO Integer
			   		      let vchanReq = VChanRequest me n1
			   		      sendHttp' (WCommRequest vchanReq) conn
			   		      eitherSharedBack <- receiveHttp conn  --this should be the challenge nonce for vchan comm. 
			   		      case eitherSharedBack of
			   		       (Left err) -> do
			   		       		      putStrLn err
			   		       		      return Nothing
					       (Right shared) -> do
					       		      case shared of
					       		        (WNonce n2) -> do   --this should be the challenge nonce for vchan comm. 
					       		        	  eitherShared <- eitherReceiveSharedWTimeout rChan
					       		        	  case eitherShared of
					       		        	    (Left err) -> do
					       		        	    		   putStrLn err
					       		        	    		   return Nothing
					       		        	    (Right shared) -> case shared of
					       		        	    			(WNonce shouldBeN1plus1) ->
					       		        	    			  if shouldBeN1plus1 == (n1 + 1) then do
					       		        	    			    --send back the nonce received over http
					       		        	    			    sChannel <- sendShared id (WNonce (n2+1))
					       		        	    			    let vchanInfo = VChanInfo (Just sChannel) (Just rChan)
					       		        	    			    let channel = Channel ent vchanInfo
					       		        	    			    return (Just channel) --yay!!!!!!!!!!! 
					       		        	    			   else do
					       		        	    			     putStrLn "Error. That's not the nonce I wanted!"
					       		        	    			     return Nothing
					       		        	    			(_) -> do
					       		        	    				putStrLn "received unexpected thing from VChannel"
					       		        	    				return Nothing
					       		        (_)   -> do
					       		                  putStrLn "Unexpected response type in vchanrequest over http"
					       		                  return Nothing			   		       		      
			   		      
			   		  --mSendChan <- maybe_client_init id	        
negotiator :: ArmoredStateTMonad ()
negotiator = do
    s <- get
    let chanETMVar =  channelEntriesTMVar s
    let me = executor s
    liftIO $ Scotty.scotty (fromIntegral negotiationport) $ do
	    Scotty.get "/" $ Scotty.text "serving\n"  --a quick way to check if the CA is 
	    				 	--serving, client should see "foobar" 
	    				 	--(Note: unqualified get is ambiguous).

	    Scotty.post "/" $ do
	      --reads in "potential" request (parsing it could fail). 
	      --Note: no en/de-crypting is yet taking place.
	      a <- (param "request") :: ActionM LazyText.Text  
	     -- myprint' ("Received (Text):\n" ++ (show a)) 2 --debug show of text.
	     -- myprint' ("Received (UTF8):\n" ++ (show (LazyEncoding.encodeUtf8 a))) 2 --debug printout.
	     -- myprint' ("Data received on port: " ++ (show port)) 1
	      
	      --first converts the Text to UTF8, then then attempts to read a CARequest
	      let jj = AD.jsonEitherDecode (LazyEncoding.encodeUtf8 a) :: Either String Shared
	      case jj of
	      	(Left err) -> text (LazyText.pack "ERROR: Improper request.")
	      	(Right (WCommRequest (VChanRequest entity n1))) -> do
	      	--1. check if channel already exists, do nothing if it does
	      	--2. if we need to make a channel..
	      	
	      	--first lock on the channels
	      	  chanls <- liftIO $ atomically $ takeTMVar chanETMVar
	      	  chanExists <- liftIO $ channelExistsWith chanls entity
	      	  case chanExists of
	      	  	(Just chan)  -> do
	      	  		  liftIO $ atomically $ putTMVar chanETMVar chanls
	      	  		  json VChanSuccess --do nuthin'
	      	  	Nothing      -> do -- interesting stuff
      	  		  maybeVChanPair <- case (entityId entity) of 
      	  				    (Nothing)  -> return Nothing
      	  				    (Just id)  -> do
      	  				    		   n2 <- liftIO (randomIO :: IO Integer)
      	  				    		   json (WNonce n2)
      	  				    		   liftIO $ attemptVChanContactR id n1 n2
      	  		  case maybeVChanPair of
      	  		   (Just (sChan,rChan)) -> do
      	  		   			let chan = Channel entity (VChanInfo (Just sChan) (Just rChan)) 
      	  		   		--armoredlsTMVar <- liftIO ( newTMVarIO [] :: IO (TMVar [Armored])) --shouldn't need this at all for vChan
      	  		   			--unitTMVar <- liftIO $ newTMVarIO () 
      	  		   			let chanEntry = ChannelEntry ("ChannelWith" ++ (entityName entity)) 
      	  		   						     chan 
      	  		   						     --armoredlsTMVar
      	  		   						     --unitTMVar
      	  		   			liftIO $ atomically $ putTMVar chanETMVar (chanEntry:chanls)
      	  		   			json VChanSuccess			     
      	  		   Nothing              -> do --then we can't do vchan and must do http.
      	  		   			liftIO $ atomically $ putTMVar chanETMVar chanls
      	  		   			json VChanFailure
      	        (Right (WCommRequest (PortRequest entity portSuggestion nonce))) -> do
      	           chanls <- liftIO $ atomically $ takeTMVar chanETMVar
	      	   mchan <- liftIO $ channelExistsWith chanls entity
	      	   case mchan of
	      	   	(Just channel) -> do
	      	   		let chanInfo = channelInfo channel
	      	   		let respThingy = case chanInfo of
	      	   				  (VChanInfo mchanS mchanR) -> case (mchanS,mchanR) of
	      	   				  				((Just _),(Just _)) -> VChanSuccess
	      	   				  				(_, _)              -> VChanFailure
	      	   				  (HttpInfo myPort _ _ _ _ _) -> HttpSuccess myPort
	      	   				  				
	      	   		liftIO $ atomically $ putTMVar chanETMVar chanls
	      	  		json $ respThingy
	      	  		--note possible attack here. send entity you want to be and it will respond with listening port.
			Nothing        -> do
				case entityIp entity of
				  Nothing -> do
				  	      liftIO $ atomically $ putTMVar chanETMVar chanls
				  	      liftIO $ putStrLn "no sender IP"
				  	      json HttpFailure
				  (Just ip) -> do
				     let takenPorts = harvestPorts chanls
				     let myport = (if portSuggestion `elem` takenPorts 
				  	            then ((maximum takenPorts) + 1)  --possible security issue. 
				  	            else portSuggestion )
				     conn <-liftIO $ openConnection ip portSuggestion
				     --TODO send nonce + 1 here.
				     armoredlsTMVar <- liftIO ( newTMVarIO [] :: IO (TMVar [Armored]))
      	  		   	     unitTMVar <- liftIO (newEmptyTMVarIO  :: IO (TMVar ()) ) --empty since obviously no messages yet.
      	  		   	     let httpInfo = HttpInfo myport portSuggestion ip (Just conn) armoredlsTMVar unitTMVar
      	  		   	     let channel = Channel entity httpInfo
      	  		   	     let channelEntry = ChannelEntry ("ChannelWith" ++ (entityName entity)) channel
      	  		   	     liftIO ( do
      	  		   	     	forkIO (httpServe httpInfo)
      	  		   	     	return () )
      	  		   	     liftIO $ atomically $ putTMVar chanETMVar (channelEntry:chanls)
      	  		   	     json $ HttpSuccess myport
				  	 	      	  		
      	           
	      	  		
	      	{-
	      		liftIO $ atomically $ do
	      		  chanls <- takeTMVar chanETMVar
	      		  state <- channelExists chanls entity
	      		  case state of
	      		   True  -> do
	      		             --putTMVar chanETMVar chanls 
	      		   	     return ()
	      		   False -> do
	      		   	     --no channel exists, lets try vChanFirst
	      		   	     
	      		   	     return ()
	      		  putTMVar chanETMVar chanls
	      		  
	      	 
	      		text (LazyText.pack "ERROR: Improper request.")
	      	(Right (WCommRequest (PortRequest entity port nonce))) -> do 
	      		text (LazyText.pack "ERROR: Improper request.")
	     -- myprint' ("JSON decoded CARequest result:\n" ++ (show jj)) 2 --debug 
              -}
                  --text (LazyText.pack "kgeojgeijgeigjg") --report read error: client 
              {-
	      case jj of
		   (Left err)    -> do
		   		     let str = "Error decoding CARequest from JSON. Error was: " ++ err
		                     --myprint' str 1    --report read error: console
		                     text (LazyText.pack str) --report read error: client
		   (Right (CommRequest req)) -> do 
		   		    
		                     
	           (Right _)       -> do 
                		     let str = ("Recieved something that wasn't a WPort. No! I refuse: " ++ err)
                                     --myprint' str 1    --report error: console
                                     text (LazyText.pack str) --report error: client
                                     
                                     
                                     -}

    return ()
httpServe :: ChannelInfo -> IO ()
httpServe (HttpInfo myport theirPort theirIP maybeConn msglsTMVar unitTMVar) = do
  let intPort = (fromIntegral(myport) :: Int)
  Scotty.scotty intPort $ do
  	    Scotty.get "/" $ Scotty.text "serving\n"  --a quick way to check if the CA is 
	    				 	--serving, client should see "foobar" 
	    				 	--(Note: unqualified get is ambiguous).

	    Scotty.post "/" $ do
	      --reads in "potential" msg (parsing it could fail). 
	      --Note: no en/de-crypting is yet taking place.
	      a <- (param "request") :: ActionM LazyText.Text  
	     -- myprint' ("Received (Text):\n" ++ (show a)) 2 --debug show of text.
	     -- myprint' ("Received (UTF8):\n" ++ (show (LazyEncoding.encodeUtf8 a))) 2 --debug printout.
	     -- myprint' ("Data received on port: " ++ (show port)) 1
	      
	      --first converts the Text to UTF8, then then attempts to read a CARequest
	      let jj = AD.jsonEitherDecode (LazyEncoding.encodeUtf8 a) :: Either String Shared
	      case jj of
		(Left err)     -> text (LazyText.pack "ERROR: Improper request.")
		(Right shared) -> do
		  let armored = sharedToArmored shared
		  liftIO $ atomically $ do 
		  			 chanls <- takeTMVar msglsTMVar
		        		 let chanls' = chanls ++ [armored]
		        		 putTMVar msglsTMVar chanls'
		        		 putTMVar unitTMVar ()  -- definitely has something now. 
		  json (HttpSuccess (myport)) -- don't know why it won't let me put an empty string in there.
		  
  return ()
  
harvestPorts :: [ChannelEntry] -> [HttpClient.Port]
harvestPorts []     = []
harvestPorts (x:xs) = case (channelInfo (channelEntryChannel x)) of
			(VChanInfo _ _)          -> harvestPorts xs
			h@(HttpInfo _ _ _ _ _ _) -> (httpInfoMyServingPort h) : (harvestPorts xs)






 
