module AppraiserProtocol where

import Protocol

attAddress = Address {
	        name= "Attester",
	        ip   = Nothing,
	        port = Nothing,
	        id   = Just 19,
	        note :: (Just "Just an attestered here to do your bidding")
	      }
appAddress = Address {
	        name= "Appraiser",
	        ip   = Nothing,
	        port = Nothing,
	        id   = Just 17,
	        note :: (Just "Just a lonely Appraiser")
	      }
pCAAddress = Address {
	        name= "PrivacyCA",
	        ip   = (Just "10.100.0.6"),
	        port = (Just 3000),
	        id   = Nothing,
	        note :: (Just "Just a lonely Privacy CA out here in the deep web")
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
	     (Let (Var "attesterChan") (ArmoredCreateChannel AMyself attester Vchan)	      
 	     (Send (Var "request") (Var "attesterChan")
	     (Receive (Var "response") (Var "attesterChan")
	     (Let (Var "finalResult") (ArmoredEvaluate (Var "request") (Var "response"))
	     (Result (Var "finalResult"))
	      )))))))
	      
	      
attProtocol = ReceiveAny (Var "receivedRequest") (Var "appChannel")
	     (Let (Var "caCertReq") (ArmoredCreateCACertReq AMyself)
             (Send (Var "caCertReq") ArmoredCAChannel
             (Receive (Var "CACert") ArmoredCAChannel)
	     (Let (Array "emptyArray") ArmoredEmptyArray
	     (Let (Array "desiredEvidence") ArmoredExtractDesiredEvidence
	     (ForEach (Array "desiredEvidence")
	        (Send ArmoredArrayItem ArmoredMeasurerChan 
	        (Receive (Var "evidence") ArmoredMeasurerChan
	        (AppendArray (Array "emptyArray") (Var "evidence"))))
	     (Let (Var "quote") ArmoredCreateQuote
	     (Let (Var "responseToApp") ArmoredCreateAppResponse
	     (Send (Var "responseToApp") (Var "appChannel")
	      Stop
	      ))))))))
	      
	      
	      
	        
	   
	   
