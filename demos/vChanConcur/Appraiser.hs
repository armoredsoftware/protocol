{-# LANGUAGE ScopedTypeVariables #-}

--vchan library
import VChanUtil

import Demo1V1
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Crypto.Random.API (cprgCreate)
import Crypto.Random (createEntropyPool)
import Data.Maybe



prompt:: IO (Int)
prompt= loop
      where loop = do putStrLn "Which Domain ID would you like to Appraise?"
                      input <- getLine
                      case reads input of
                        [(id,_)] -> return id
                        _     -> do putStrLn "Error: Please Enter a Number."
                                    loop

getRequest :: TMVar Shared -> STM Shared
getRequest m = 
	   do r <- readTMVar m
	      case r of
	       Appraisal _ -> return r
	       otherwise -> retry

putQuote :: TMVar Shared -> Shared -> STM ()
putQuote m quo = do void $ swapTMVar m quo

spawnVChan :: TMVar Shared -> IO ()
spawnVChan m = 
  
  do id <-getDomId
     putStrLn $ "Appraiser Domain id: "++(show id)
     other <- prompt
     chan <- client_init other
     req <- atomically $ getRequest m
     putStrLn $ "Appraiser Sending: "++(show req)
     send chan req
     ctrlWait chan
     res :: Shared<- receive chan
     putStrLn $ "Appraiser Received: "++(show res)
     atomically $ putQuote m res

-- The fun stuff
main :: IO ()
main = 
    do  m <- newEmptyTMVarIO
	forkIO $ spawnVChan m
	spawnAppraisal m
	result <- atomically (getResult m)
	case result of True -> putStrLn "Appraisal Succeeded"
	     	       False -> putStrLn "Appraisal Failed"

  where getResult :: TMVar Shared -> STM Bool
        getResult m =
            do v <- readTMVar m
               case v of
                 Result res -> return res
                 otherwise -> retry
          






{- Think of an appraisal as the three step process we've talked about:
   1)  Send a request.
   2)  Receive a quote.
   3)  Evaluate.

   For a shallow embedding, each step should be it's own function, for clarity.
   Note that we use the Either monad to propogate errors.
-}
{-
spawnAppraisal :: TMVar Shared -> IO ()
spawnAppraisal m =
  let req = mkRequest [0..7] in
    do atomically $ do cond <- tryPutTMVar m $ Appraisal req
                       when (not cond) . throwSTM $ 
                         ErrorCall "Target not ready for request."
       sq <- atomically $ getSignedQuote m
       atomically $ do result <- evaluate req sq
                       void . swapTMVar m $ Result result  
  where getSignedQuote :: TMVar Shared -> STM Quote
        getSignedQuote m  = 
            do v <- readTMVar m
               case v of
                 Attestation sq -> return sq
                 otherwise -> retry
-}
