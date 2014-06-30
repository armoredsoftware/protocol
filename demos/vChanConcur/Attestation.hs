{-# LANGUAGE ScopedTypeVariables #-}

-- vchan library
import VChanUtil

import Demo1V1
--import Demo1Utils
import Data.Maybe
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

prompt:: IO (Int)
--prompt = return 6
prompt= loop
      where loop = do putStrLn "Which Domain ID is the Appraiser?"
                      input <- getLine
                      case reads input of
                        [(id,_)] -> return id
                        _     -> do putStrLn "Error: Please Enter a Number."
                                    loop




putRequest :: TMVar Shared -> Shared -> STM ()
putRequest m req = putTMVar m req

getQuote :: TMVar Shared -> STM Shared
getQuote m = 
	 do q <- readTMVar m
	    case q of 
	      Attestation _ -> return q
	      otherwise -> retry

spawnVChan :: TMVar Shared -> IO ()
spawnVChan m = 
    do id<- getDomId
       --let priKey = getPriKey
       putStrLn $ "Domain id: "++(show id)
       appraiser<- prompt
       chan <- server_init appraiser
       ctrlWait chan
       val:: Shared <-receive chan
       putStrLn $ "Appraisal Received: "++(show val)
       case val of Appraisal app -> putStrLn $ show app
       	    	   otherwise -> putStrLn "Not Appr"
       --putStrLn $ "HERe"
       atomically $ putRequest m (val::Shared)
       putStrLn $ "putRequest"
       quote <- atomically $ getQuote m
       send chan quote
--     ctrlWait chan
       putStr ""       


-- The fun stuff
main :: IO ()
main = 
    do m <- newEmptyTMVarIO
       forkIO $ spawnVChan m
       spawnAttestation m
       return ()
      {- id<- getDomId
       let priKey = getPriKey
       putStrLn $ "Domain id: "++(show id)
       appraiser<- prompt
       chan <- server_init appraiser
       ctrlWait chan
       val:: Shared <-receive chan
       putStrLn $ "Attestation Received: "++(show val)
--       let sQuote = mkSignedQuote priKey val
--       putStrLn $ "Attestation Sending: "++(show sQuote) 
--       send chan  sQuote
--       ctrlWait chan
       putStr ""
       -}
    
 --      close chan
