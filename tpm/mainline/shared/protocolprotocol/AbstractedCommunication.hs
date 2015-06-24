{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, OverlappingInstances, OverloadedStrings, RecordWildCards, ExistentialQuantification #-}
module AbstractedCommunication where

import Data.Aeson
import qualified ProtoTypes as P 
import Control.Applicative
import Control.Concurrent (ThreadId)
import CommTools hiding (sendHttp, receiveHttp)
import System.Random
import Control.Concurrent.MVar
import Control.Concurrent.STM.TMVar
import Control.Concurrent.STM
import Control.Monad.State.Strict
import Control.Monad
import CommunicationNegotiator (defaultport)
import System.Timeout (timeout)
import Control.Concurrent
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text.Lazy.Encoding as LazyEncoding


import qualified Demo3Shared as AD
import Data.ByteString.Lazy hiding (putStrLn,length,map)

defaultTimeout = 10000000 :: Int  -- 10 seconds 
shortTimeout   = 10000000 :: Int  -- 10 second 
class IsChannel a where
    send ::  (IsMessage b) => a -> b -> IO Bool
    --should add parameter of an IO computation to do in case of a receive error. 
    --come to think of it, probably could use one for send as well.
    receive :: (IsMessage b) => a -> IO (Result b)
    initialize :: a -> IO ()
    initFail :: a -> String
    killChan :: a -> IO ()
    chanTypeOf :: a -> String
    toRequest :: a -> IO Value
    fromRequest :: Value -> a -> IO (Either String a)
    negotiation ::  a -> IO a
    defaultChan :: IO a 
    
    negotiation c = defaultChan 


data Channel = forall a. (IsChannel a) => Channel {
     internalChan :: a,
     receiveThreadID :: Maybe ThreadId,
     getMessages :: (TMVar [Value]),
     getUnitMVar :: (MVar ())
     }

instance IsChannel (Channel) where
 send (Channel a _ _ _) = send a
 receive (Channel a _ tmvar mvarUnit) = do 
                                putStrLn "&"
                                mUnit <- timeout defaultTimeout (takeMVar mvarUnit)
                                case mUnit of 
                                  Nothing -> return $ Error $ "Error: Timeout in receive. Waited ~" ++ (show (defaultTimeout `div` 1000000)) ++ " seconds."
                                  Just _  -> do 
                                    (m:ls') <- liftIO $ atomically $ takeTMVar tmvar  
                                    liftIO $ atomically $ putTMVar tmvar ls' --regardless of what is left, put it back. 
                                    if length ls' == 0
                                     then return False --note that we have already taken the unitMVar. We simply make note here that it is still empty. false to match
                                     else tryPutMVar mvarUnit () --non-blocking
                                    return (fromJSON m)
 initFail (Channel a _ _ _) = initFail a
 initialize (Channel a _ _ _) = initialize a 
 killChan (Channel a tid _ _) = do 
   killChan a
   case tid of 
     Nothing -> return () 
     Just tid' -> killThread tid'
 chanTypeOf (Channel a _ _ _) = chanTypeOf a 
 toRequest (Channel a _ _ _) = toRequest a --(internalChan c)
 fromRequest v (Channel a b c d) = do 
   elilChan <- (fromRequest v a)  --(internalChan c)
   case elilChan of 
     Left err -> return (Left err)
     Right lilc -> do 
       --c <- mkChannel lilc 
       return $ Right (Channel lilc b c d) 
 negotiation (Channel a _ _ _ ) = do 
  c <- negotiation (a)
  mkChannel c 




declareCommunication :: (Channel, [Channel]) ->(Channel -> IO a) -> IO ()
declareCommunication (mc@(Channel a _ _ _),chls) f =  do
 forever $ do
  --mv <- newEmptyMVar
  msg <- receive a :: IO (Result (Value,Integer))   -- forkIO $ stutterReceive mc mv 
--  msg <- takeMVar mv
  case  msg of 
   Error err -> do 
    putStrLn $ "improper comm request. not a pair!: " ++ err 
   Success (req,nonce) -> do 
     let ls = map (fromRequest req) chls  
     ls' <- sequence ls 
     case fstRight ls' of 
       Nothing -> do 
        putStrLn "no matching channels"
       Just chan -> do
        echan <- bChannelInit chan nonce 
        case echan of 
         Left err -> do 
          putStrLn $ "Error in bchannel init: " ++ err 
         Right chan' -> do 
          f chan
          putStrLn "success"


fstRight :: [Either a b] -> Maybe b 
fstRight [] = Nothing 
fstRight ((Left _): xs) = fstRight xs 
fstRight ((Right b):_) = Just b 

stutterReceive ::Channel -> MVar Value -> IO () 
stutterReceive c@(Channel a _ _ _ ) mv = do
  putStrLn "stutter Receive"
  tj <- toRequest c  
  --putMVar mv (toJSON tj )
  msg <- receive a :: IO (Result Value)
  case msg of 
   Error err -> do 
     putStrLn err 
   Success v -> do  
    putStrLn $ "Stutter receive got a message!!!: " ++ (show v)
    putMVar mv v
    return ()
  stutterReceive c mv 


mkChannel :: (IsChannel a) => a ->IO Channel
mkChannel littleChan = do 
  initialize littleChan
  putStrLn "MKCHAN"
  tmvar <- newTMVarIO [] 
  mvar  <- newEmptyMVar
  let chan = Channel littleChan Nothing tmvar mvar 
  tid <- forkIO $ superReceive chan
  return (Channel littleChan (Just tid) tmvar mvar )

mkChannel' :: (IsChannel a) => a -> IO Channel
mkChannel' lil = do 
 tmvar <- newTMVarIO []
 mvar <- newEmptyMVar
 return (Channel lil Nothing tmvar mvar)

superReceive :: Channel -> IO ()
superReceive c@(Channel a _ msgTMVar unitMVar) = do 
  putStrLn "%" 
  msg <- receive a :: IO (Result Value)
  putStrLn "^^^^^^^^^^^^"
  case msg of 
   Error err -> do 
     putStrLn $ "Error superReceive: " ++ err 
   Success val -> do 
     putStrLn $ "SUPER RECEIVED: " ++ (show val) 
     _ <- tryTakeMVar unitMVar 
     msgls <- atomically $ takeTMVar msgTMVar 
     atomically $ putTMVar msgTMVar (msgls ++ [val])
     putMVar unitMVar ()
   {-  case (fromJSON val) :: Result CommRequest of 
       (Error _) -> do
             return () 
       Success _ ->  do 
             forkIO  $ do 
               bChannelInit c c
               return ()
             return () -}
  --putStrLn "end super receive" 
  superReceive c 
  

type IsMessage a = (ToJSON a, FromJSON a)
--data Message = forall b. IsMessage b => Message b 
--instance ToJSON Message where
--  toJSON (Message b) = toJSON b 
--instance FromJSON Message where
--  parseJSON =  parseJSON 
 


{- 
data CommRequest = CommRequest {
      requester :: P.Entity,
      reqPort   :: HttpClient.Port,
      --reqNonce  :: Integer,
      reqChanType :: String
      } deriving (Eq, Show)
instance ToJSON CommRequest where
  toJSON x = object
   [ "Requester" .= requester x
   , "ReqPort"   .= reqPort x 
  --- , "ReqNonce"  .= reqNonce x
   , "ReqChanType" .= reqChanType x
   ]
instance FromJSON CommRequest where
  parseJSON (Object o) = CommRequest <$> o .: "Requester"
                                     <*> o .: "ReqPort"
                                   --  <*> o .: "ReqNonce"
                                     <*> o .: "ReqChanType"
-}
newtype CommResponse = CommResponse (Integer,Integer) deriving (Show, Eq)

instance ToJSON CommResponse where
  toJSON (CommResponse pair) = object
    [ "Nonces" .= toJSON pair]
instance FromJSON CommResponse where
  parseJSON (Object o) = CommResponse <$> o .: "Nonces"
                          
--mkMessage :: (IsMessage b) => b -> Message
--mkMessage b = Message b 

mkCommRequest :: (IsChannel a) => a -> IO (Value, Integer)
mkCommRequest c = do 
  reqv <- toRequest c 
  nonce <- liftIO (randomIO :: IO Integer)
  return (reqv,nonce)

--second channel gets ammended into channel we return. 
aChannelInit :: Channel -> Channel -> IO (Either String (Channel))
aChannelInit neg c = do 
  putStrLn "beginning achannelInit"
  (req,n1) <- mkCommRequest c 
  putStrLn "made it here"
  send neg (req,n1) 
  info <- receive c :: IO (Result (Value,Integer,Integer))
  case info of 
   Error err -> do 
     putStrLn err 
     return (Left err)
   Success (reqv, n1',nb1) -> do 
     ec <- fromRequest reqv c 
     case ec of 
      Left err -> do 
        putStrLn err 
        return $ Left err
      Right c' -> do
      -- n1_2 <- (randomIO :: IO Integer)
       if n1' == n1+1 then do 
         send c' (CommResponse (nb1+1,0))
         return $ Right c' 
         else do 
         return $ Left "nonces no match!!"
{-
       resp <- (receive c :: IO (Result CommResponse))       
       case resp of 
         Error err -> do 
           putStrLn err 
           return $ Left err 
         Success (CommResponse (n1',n1_2'))-> do
           if ((n1 + 1) == n1') && ( (n1_2 + 1) == n1_2')
              then do     
                bc <- mkChannel c'            
                return $ Right bc  --han 
              else do 
                putStrLn $ "mismatch: " ++ (show n1') ++ " and " ++ (show (n1 + 1))
                return $ Left "n1' did not match n1."
           -}

bChannelInit :: Channel ->Integer -> IO (Either String (Channel))
bChannelInit c na = do 
     putStrLn "bChannelInit triggered!!"
  --comReq <- (receive neg :: IO (Result (Value,Integer)))
 -- case comReq of 
--   Error err -> do
 --     putStrLn err 
--      return $ Left err
--   Success (comReq',na) -> do 
     nb1 <- randomIO :: IO Integer 
     respReq <- toRequest c 
     send c (respReq,na+1,nb1)
     
     --send c (CommResponse (na + 1, nb1))
     resp <-  (receive c :: IO (Result CommResponse))
     case resp of 
       Error err -> do 
         putStrLn err 
         return $ Left err 
       Success (CommResponse (nb1',n2))-> do
         if (nb1 + 1) == nb1' 
          then do
             --send c (CommResponse (na+1,n2+1))
             --chan <- mkChannel c 
             return $ Right c --han 
           else do 
             return $ Left "n1' did not match n1."   
















f :: (IsMessage a) => a -> IO ()
f m = do putStrLn "hello"


data T = T deriving (Show)
--instance IsMessage T 



instance ToJSON T where
  toJSON T = toJSON P.OS
instance FromJSON T where
  parseJSON o = pure T

--class (Monad a) => IsNegotiationMonad a where 
  --    sendM :: (IsChannel b, IsMessage c) => a -> b -> c -> IO Bool


{--
instance Monad Process where
 a >>= b = 


--} 

jsonEncode :: (ToJSON a) => a -> ByteString
jsonEncode = Data.Aeson.encode

jsonParse :: (FromJSON a) => ByteString -> Result a
jsonParse bs =case Data.Aeson.eitherDecode bs of 
                Left err -> Error err
                Right x  -> Success x

jsonDecode :: (FromJSON a) => ByteString -> Maybe a
jsonDecode= Data.Aeson.decode