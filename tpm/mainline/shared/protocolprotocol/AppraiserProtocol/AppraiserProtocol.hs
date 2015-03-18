{-# LANGUAGE OverloadedStrings #-}

module AppraiserProtocol where

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S

import Protocol
import ProtoTypes
import Network.Http.Client
import Demo3Shared hiding (Result)
import Control.Concurrent.STM.TMVar
import CommunicationNegotiator
import Control.Concurrent
import Control.Monad
import Control.Monad.State.Strict
{-
appAddress = Address {
	        name= "Appraiser",
	        ip   = (Just "10.100.0.246"),
	        port = (Just 3000),
	        getid   = Just 1,
	        note = (Just "Just a lonely Appraiser")
	      }
	      
attAddress = Address {
	        name= "Attester",
	        ip   = (Just "10.100.0.208"),
	        port = (Just 3000),
	        getid   = Just 2,
	        note = (Just "Just an attestered here to do your bidding")
	      }

pCAAddress = Address {
	        name= "PrivacyCA",
	        ip   = (Just "10.100.0.6") :: (Maybe Hostname),
	        port = Just 3000,
	        getid   = Nothing,
	        note = (Just "Just a lonely Privacy CA out here in the deep web")
	      }	
	      
app = Appraiser appAddress
att = Attester attAddress
pca = PrivacyCA pCAAddress
-}	      
appraise = do
	    putStrLn "Appraise be to Attester"
	    let knownguys = [att,pCA]
	    let emptyvars = []
	    emptyTMVarChans <- newTMVarIO []
	    let me = app
	    let s0 = ArmoredState emptyvars me knownguys emptyTMVarChans
	    forkIO ( do 
	    		runStateT negotiator s0
	    		return ()
	    	   )
	    threadDelay 6000000
	    runExecute' myProto s0
	    
myProto =     CreateChannel (AChannel "attesterChan") (AEntity att)
	     (Let (Var "evDes") (AEvidenceDescriptor D0)	      
 	     (Send (Var "evDes") (AChannel "attesterChan")
	     (Receive (Var "response") (AChannel "attesterChan")
	     (Result (Var "response"))
	      )))
	   
	  -- -}
