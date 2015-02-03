{-# LANGUAGE OverloadedStrings #-}

module AttesterProtocol where

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S

import Protocol
import Network.Http.Client
import Demo3Shared hiding (Result)

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
	      
attest = do
	    putStrLn "Appraise be to Attester"
	    let knownguys = [att,pca]
	    let emptyvars = []
	    let emptychans = []
	    let me = app
	    let s0 = ArmoredState emptyvars me knownguys emptychans
	    runExecute' myProto s0
	    
myProto =     CreateChannel (AChannel "chan") AMyself (AEntity app) (ACommMethod Http)
	     (Receive (Var "mess") (AChannel "chan")	      
 	     (Send (Var "mess") (AChannel "chan")
	     (Result (Var "response"))
	      ))
	      
	      -}
