{-# LANGUAGE ScopedTypeVariables #-}

-- vchan library
import VChanUtil

import Demo1V1
import Data.Maybe
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

prompt:: IO (Int)
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
       putStrLn $ "Attester Domain id: "++(show id)
       appraiser<- prompt
       chan <- server_init appraiser
       ctrlWait chan
       val:: Shared <-receive chan
       case val of Appraisal app 
       	    	   	     -> putStrLn $ "Appraisal Received: "++(show val)
       	    	   otherwise -> putStrLn "Not Appr"
       atomically $ putRequest m (val::Shared)
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
