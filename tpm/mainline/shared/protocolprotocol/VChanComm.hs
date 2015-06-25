{-# LANGUAGE RankNTypes, TypeSynonymInstances, FlexibleInstances, ConstraintKinds, OverlappingInstances, OverloadedStrings, RecordWildCards, ExistentialQuantification #-}
module VChanComm where


import AbstractedCommunication
import Data.Aeson
import VChanUtil
import Control.Concurrent.MVar
import Data.ByteString.Lazy hiding (putStrLn,length,map)
--import CommTools
import Data.IORef
import Control.Applicative
data VChannel = VChannel {
     refmTheirID :: IORef (Maybe Int),
     reflibXenVChan :: IORef (Maybe LibXenVChan)
     }


instance IsChannel VChannel where
  send c m = do 
    mchan <- readIORef (reflibXenVChan c)
    case mchan of 
     Nothing -> return False
     Just chan -> do
        logger <- createLogger
        sendChunkedMessageByteString logger chan (toStrict (Data.Aeson.encode m))
        return True
  receive c = do
    mchan <- readIORef (reflibXenVChan c)
    case mchan of 
      Nothing -> return $ Error "ummm.... the libxenvchan didn't exist!"
      Just chan -> do 
        ctrlWait chan
        logger <- createLogger
        bytes <- readChunkedMessageByteString logger chan
        return $ jsonParse (fromStrict bytes)
  initialize c = do 
    mTheirID <- readIORef ( refmTheirID c )
    case (mTheirID ) of 
     Nothing -> return () 
     Just idd -> do 
       mchan <- readIORef (reflibXenVChan c)
       case mchan of 
         Nothing -> do 
           libchan <- server_init idd
           modifyIORef' (reflibXenVChan c) 
            (\mchan -> case mchan of 
                         Nothing -> Just libchan
                         Just x -> Just x ) 
         Just _ -> return ()
  killChan c = do 
    mchan <- readIORef (reflibXenVChan c)                              
    case mchan of 
      Nothing -> return () 
      Just chan -> do close chan 
  toRequest c = do 
    idd <- getDomId 
    return $ toJSON $ VChanRequest idd
  fromRequest r c = do
    case fromJSON r :: Result VChanRequest of 
      Error err -> return $ Left err
      Success (VChanRequest i) ->do 
       modifyIORef (refmTheirID c) (\_ -> Just i)
       return $ Right c  
newtype VChanRequest = VChanRequest {
  id :: Int
  } deriving (Show, Eq)
instance ToJSON VChanRequest where
  toJSON (VChanRequest x) = object
    [ "VChanRequestID" .= x]

instance FromJSON VChanRequest where
  parseJSON (Object o) = VChanRequest <$> o .: "VChanRequestID"    


{-
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
-}