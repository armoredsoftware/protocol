{-# LANGUAGE OverloadedStrings #-}

module Main where 

import Web.Scotty
import qualified Demo3SharedNOVCHAN as AD
import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S

import Network.Http.Client
import qualified Network Http.Client as HttpClient
import Data.ByteString.Lazy (ByteString, pack, append, empty, cons, fromStrict, length)

import AttesterCAComm
import ProtoTypes
negotiationport = 3000
 
 
requestPort :: Http.Client.Hostname -> IO (Either String HttpClient.Port)
requestPort ip = do
		  chan <- sendHttp (WPort 2999) ip negotiationport
		  eitherShared <- receiveHttp chan
		  case eitherShared of
		   (Left err)           -> return (Left err)
		   (Right (WPort port)) -> return (Right port)
		   (Right _             -> return (Left "that's weird.. received something that's not a port..")
		   

portNegotiator :: ArmoredStateTMonad ()
portNegotiator = do
    s <- get
    let chanlsTMVar = channelpairsTMVar s
    liftIO $ scotty negotiationport $ do
	    Web.Scotty.get "/" $ text "CA serving\n"  --a quick way to check if the CA is 
	    				 	--serving, client should see "foobar" 
	    				 	--(Note: unqualified get is ambiguous).

	    post "/" $ do
	      --reads in "potential" request (parsing it could fail). 
	      --Note: no en/de-crypting is yet taking place.
	      a <- (param "request") :: ActionM LazyText.Text  
	     -- myprint' ("Received (Text):\n" ++ (show a)) 2 --debug show of text.
	     -- myprint' ("Received (UTF8):\n" ++ (show (LazyEncoding.encodeUtf8 a))) 2 --debug printout.
	     -- myprint' ("Data received on port: " ++ (show port)) 1
	      
	      --first converts the Text to UTF8, then then attempts to read a CARequest
	      let jj = AD.jsonEitherDecode (LazyEncoding.encodeUtf8 a) :: Either String AD.WPort
	     -- myprint' ("JSON decoded CARequest result:\n" ++ (show jj)) 2 --debug 

	      case jj of
		   (Left err)    -> do
		   		     let str = "Error decoding CARequest from JSON. Error was: " ++ err
		                     --myprint' str 1    --report read error: console
		                     text (LazyText.pack str) --report read error: client
		   (Right (WPort p)) -> do 
		   		     --myprint' ("Successfully parsed request from JSON:\n" ++ (show caReq)) 1
		                     --json :: ToJSON a => a -> ActionM ()
		                     --Set the body of the response to the JSON encoding of the given value. 
		                     --Also sets "Content-Type" header to "application/json; charset=utf-8" if it has not already been set.
		                     --json caReq  --I think we can delete this. was for early testing (echoing). -paul
		                     
		                     --now that we have succesfully parsed a CARequest, let us attempt to formulate a CAResponse.
		                     
		                     chanls <- takeTMVar chanlsTMVar
		                     let portls = getPorts chanls 
		                     let portNum = (case lookup p portls of
		                        (Nothing)      -> p
		                        (Just found) -> (maximum portls) + 1 )
		                     
		                       
		                      		  --create channel and start listening here.
		                      		  
		                     eitherCAResp <- liftIO $ handleCAReq caReq
		                     case eitherCAResp of
		                        (Left err) 	-> do
		                        		     let str = ("Error formulating CAResponse. Error was: " ++ err)
		                                             --myprint' str 1    --report error: console
		                                             text (LazyText.pack str) --report error: client
		                        (Right caResp) 	-> do
		                        		     myprint' "Sending response" 1
		                                             json caResp 
	           (Right _)       -> do 
                		     let str = ("Recieved something that wasn't a WPort. No! I refuse: " ++ err)
                                     --myprint' str 1    --report error: console
                                     text (LazyText.pack str) --report error: client
	           		     
getPorts :: [(String, (Channel, MVar [Armored]) )] -> [HttpClient.Port] -> [HttpClient.Port]
getPorts [] ports = ports
getPorts ((_,(Channel _ _ (HttpInfo _ port _))):xs) ports = getPorts xs (port:ports)
getPorts (_:xs) ports = getPorts xs ports

