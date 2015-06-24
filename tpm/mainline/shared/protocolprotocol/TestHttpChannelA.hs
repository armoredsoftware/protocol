{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, OverlappingInstances, OverloadedStrings, RecordWildCards, ExistentialQuantification #-}
module TestHttpChannelA where
import Control.Concurrent
import HttpChannel
import AbstractedCommunication 
myMain :: IO ()
myMain = do
     lilNeg <- defaultChan :: IO HttpChannel
--  r <- toRequest lilNeg
--  lilNeg' <- fromRequest r lilNeg --this puts their port to our serving port
--  case lilNeg' of 
--   Left err -> do 
--     putStrLn $ "Error in creating fromReq of neg chan: " ++ err 
--   Right (HttpChannel {..}) -> do 
     comneg <- mkChannel lilNeg --(HttpChannel { httpchanTheirIp = (Just "10.100.0.6"), ..})
     dumchan <- defaultChan :: IO HttpChannel
     emptyChan <- mkChannel' dumchan
     forkIO $ declareCommunication (comneg, [emptyChan]) (\x -> putStrLn "succ")
     lilNeg2 <- defaultChan :: IO HttpChannel
     r2 <- toRequest lilNeg
     lilNeg2' <- fromRequest r2 lilNeg2 --this puts their port to our serving port
     case lilNeg2' of 
       Left err -> do 
         putStrLn $ "Error in creating fromReq of neg chan: " ++ err 
       Right (HttpChannel {..}) -> do 
         comneg2 <- mkChannel (HttpChannel { httpchanTheirIp = (Just "10.100.0.6"),httpchanMyServingPort=55999, ..})
         (HttpChannel {..}) <- defaultChan :: IO HttpChannel
         let channelToBe = HttpChannel { httpchanMyServingPort= 55558,
                               httpchanTheirIp=(Just "10.100.0.6"),
                               httpchanTheirServingPort=Nothing,..}
         bigChantobe <- mkChannel channelToBe            
         echan <- aChannelInit comneg2 bigChantobe
         case echan of 
           Left err -> do 
             putStrLn err 
           Right chan -> do 
             putStrLn "success on A side!!!!!!!!"



  