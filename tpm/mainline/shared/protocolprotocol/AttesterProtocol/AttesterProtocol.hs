{-# LANGUAGE OverloadedStrings #-}

module AttesterProtocol where

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.ByteString as S

import Protocol
import ProtoTypes
import Network.Http.Client
import Demo3Shared hiding (Result)
import Control.Concurrent.STM.TMVar
import Control.Monad
import Control.Monad.State.Strict
import CommunicationNegotiator
import Control.Concurrent
import Data.List
import Control.Concurrent.STM
  
attest = do
	    putStrLn "Appraise be to Attester"
	    let knownguys = [att,pCA]
	    let emptyvars = []
	    emptychans <- newTMVarIO []
            t <- newEmptyMVar
	    let me = att
	    let s0 = ArmoredState emptyvars me knownguys [] Nothing t emptychans
	    forkIO ( do
	    	runStateT negotiator s0
	    	return ()
	    	)
	    --threadDelay 6000000
	    --runExecute' myProto s0
	    newChannelTrigger emptychans []


newChannelTrigger :: TMVar [ChannelEntry] -> [ChannelEntry] -> IO a
newChannelTrigger chanETMVar handled = do 
  chanELS <- atomically $ takeTMVar chanETMVar
  let unhandled = chanELS \\ handled
  putStrLn $ (show unhandled)
  theadIDs <- sequence $ map (\chan -> do 
        forkIO (
          do 
            putStrLn "Appraise be to Attester. Poppin' a thread."
	    let knownguys = [att,pCA]
	    let emptyvars = []	   
            t <- newEmptyMVar
	    let me = att
            atomically $ tryPutTMVar chanETMVar chanELS
	    let s0 = ArmoredState emptyvars me knownguys privacyPol (Just chan) t chanETMVar  
            runExecute' myProto s0
            return ())) unhandled 
  let handled' = unhandled ++ handled
  atomically $ tryPutTMVar chanETMVar chanELS
  yield
  threadDelay 1000000 -- 1 second delay
  newChannelTrigger chanETMVar handled'
  
myProto =    (CreateChannel (AChannel "chan") Requester
	     (Receive (Var "request") Requester
             (ComputeCounterOffer (Var "counterOffer") (Var "request")	      
 	     (Send (Var "counterOffer") (AChannel "chan")
             (Receive (Var "theirFinalChoice") (AChannel "chan")
             (CheckFinalChoice (Var "finalAgreement") (Var "theirFinalChoice")
             (Send (Var "finalAgreement") (AChannel "chan")
             (HandleFinalChoice (Var "result") (Var "finalAgreement") 
	     (Result (Var "result"))
	      ))))))))

privacyPol = [Reveal [(ProtocolItem, [IntProperty 1])] FREE 
             ]
	      
--	      -}

data T1 = T1 T2 deriving (Show)

data T2 = T2 T1 deriving (Show)

f :: T1
f = T1 (T2 f)