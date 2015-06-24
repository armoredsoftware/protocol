{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, OverlappingInstances, OverloadedStrings, RecordWildCards, ExistentialQuantification #-}
module TestHttpChannelB where

import System.Random
import HttpChannel
import AbstractedCommunication 
myMain :: IO ()
myMain = do
     lilNeg <- defaultChan :: IO HttpChannel
--  r <- toRequest lilNeg
  --lilNeg' <- fromRequest r lilNeg --this puts their port to our serving port
 -- case lilNeg' of 
 --  Left err -> do 
 --    putStrLn $ "Error in creating fromReq of neg chan: " ++ err 
 --  Right lilNeg'' -> do 
     comneg <- mkChannel lilNeg --''
     dumchan <- defaultChan :: IO HttpChannel
     c <- defaultChan :: IO HttpChannel
     let c' = HttpChannel (httpchanThreadID c)
                          (55558)
                          (httpchanTheirServingPort c)
                          Nothing --(Just "10.100.0.6")
                          Nothing
                          (mvarMess c)
     c'' <- mkChannel c'
     --emptyChan <- mkChannel' dumchan
     declareCommunication (comneg, [c'']) (\x -> putStrLn "succ")
     putStrLn "success on A side!!"
     {-
      
     putStrLn "About to start bChannelInit"
    -- echan <- bChannelInit comneg c'' n1
    -- case echan of 
    --  Left err -> do 
    --    putStrLn err 
    --  Right chan -> do 
    --    putStrLn "success on A side!!!!!!!!" -}
    
  