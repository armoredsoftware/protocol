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
	    let s0 = ArmoredState emptyvars me knownguys t emptychans
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
	    let s0 = ArmoredState emptyvars me knownguys t chanETMVar  
            runExecute' myProto s0
            return ())) unhandled 
  let handled' = unhandled ++ handled
  atomically $ tryPutTMVar chanETMVar chanELS
  yield
  threadDelay 1000000 -- 1 second delay
  newChannelTrigger chanETMVar handled'
  
myProto =     CreateChannel (AChannel "chan") (AEntity app)
	     (Receive (Var "mess") (AChannel "chan")	      
 	     (Send (Var "mess") (AChannel "chan")
	     (Result (Var "mess"))
	      ))
	      
--	      -}
