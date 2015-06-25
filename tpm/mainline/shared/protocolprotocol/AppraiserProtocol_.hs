module AppraiserProtocol where

import Protocol

--TODO
--armoredPCRSel :: [Word8] --> tpm_pcr_selection

protocol = Let (Var "pcrsel") (ArmoredPCRSel [0..23]) 
	   (Let (Var "nonce") ArmoredCreateNonce 
	    (Let (Var "desiredEvidence") (ArmoredCreateDesiredEvidence [0..2])
	     (Let (Var "request") (ArmoredRequeset (Var "pcrsel") 
	     					   (Var "nonce")
	     					   (Var "desiredEvidence")
	     					   (Var "request"))
	      (Let (Var "attesterChan") (ArmoredCreateChannel AMyself attester Vchan)	      
	       (Send (Var "request") (Var "attesterChan")
	        (Receive (Var "response") (Var "attesterChan")  
