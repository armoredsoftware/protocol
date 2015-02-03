{-# LANGUAGE OverloadedStrings #-}

module ProtocolDefinitions where


import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S

import Protocol
import Network.Http.Client
import Data.ByteString.Lazy (ByteString, pack, append, empty, cons, fromStrict, length)
attAddress = Address {
	        name= "Attester",
	        ip   = Nothing,
	        port = Nothing,
	        getid   = Just 19,
	        note = (Just "Just an attestered here to do your bidding")
	      }
appAddress = Address {
	        name= "Appraiser",
	        ip   = Nothing,
	        port = Nothing,
	        getid   = Just 17,
	        note = (Just "Just a lonely Appraiser")
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
	            	      
appProtocol = Let (Var "pcrsel") (ArmoredPCRSel [0..23]) 
	     (Let (Var "nonce") ArmoredCreateNonce 
	     (Let (Var "desiredEvidence") (ArmoredCreateDesiredEvidence [0..2])
	     (Let (Var "request") (ArmoredRequesetForAttest (Var "pcrsel") 
	     	  				            (Var "nonce")
	     					            (Var "desiredEvidence"))
	     (CreateChannel (AChannel "attesterChan") AMyself (AEntity att) (ACommMethod VChan)	      
 	     (Send (Var "request") (AChannel "attesterChan")
	     (Receive (Var "response") (AChannel "attesterChan")
	     (Let (Var "finalResult") (ArmoredEvaluate (Var "request") (Var "response"))
	     (Result (Var "finalResult"))
	      )))))))
	      
	      
attProtocol = Receive (Var "receivedRequest") (AChannel "appChannel")
	     (Let (Var "caCertReq") (ArmoredCreateCACertReq AMyself)
             (Send (Var "caCertReq") ArmoredCAChannel
             (Receive (Var "CACert") ArmoredCAChannel
	     (Let (Array "emptyArray") ArmoredEmptyArray
	     (Let (Array "desiredEvidence") ArmoredExtractDesiredEvidence
	     (ForEach (Array "desiredEvidence")
	        (Send ArmoredArrayItem ArmoredMeasurerChan 
	        (Receive (Var "evidence") ArmoredMeasurerChan
	        (AppendArray (Array "emptyArray") (Var "evidence") Stop)))
	     (Let (Var "quote") ArmoredCreateQuote
	     (Let (Var "responseToApp") ArmoredCreateAppResponse
	     (Send (Var "responseToApp") (AChannel "appChannel")
	      Stop
	      )))))))))
	      
	      
	      
	        
	   
	   
