{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, OverlappingInstances, OverloadedStrings, RecordWildCards  #-}

module Demo3Vchan where

import TPM



import qualified Data.ByteString as B (ByteString)
import Codec.Crypto.RSA hiding (sign, verify)
import System.Random
import Crypto.Cipher.AES

import Data.Aeson (toJSON, parseJSON, ToJSON,FromJSON, object , (.=), (.:) )
import qualified Data.Aeson as DA (Value(..), encode, decode, eitherDecode)
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64 as Base64
import qualified Data.Text as T
import Control.Applicative ( (<$>), (<*>), pure )
import qualified Data.HashMap.Strict as HM (member, lookup)
import Data.Maybe
import qualified Data.ByteString.Char8 as Char8

import Demo3SharedNOVCHAN
import VChanUtil
import Data.Binary
import Data.ByteString.Lazy(ByteString, empty, append, pack, toStrict, fromStrict)
sendShared :: Int -> Shared -> IO LibXenVChan
sendShared id shared = do
			chan <- client_init id
			sendShared' chan shared

sendShared' :: LibXenVChan -> Shared -> IO LibXenVChan
sendShared' chan shared = do
			   logger <- createLogger
			   sendChunkedMessageByteString logger chan (toStrict (jsonEncode shared))
			   return chan

receiveShared :: LibXenVChan -> IO (Either String Shared)
receiveShared chan = do
			ctrlWait chan
			logger <- createLogger
			bytes <- readChunkedMessageByteString logger chan
			let shared =  jsonEitherDecode (fromStrict bytes) :: Either String Shared
			return shared


sendM :: (Binary a, Show a) => String -> LibXenVChan ->  a -> IO ()
sendM descrip chan m = do
  putStrLn $ descrip ++ "Sending: " ++ show m ++ "\n"
  send chan $ m
  return () 


sendR :: (Binary a, Show a) => PlatformID -> String -> a -> IO LibXenVChan
sendR dest descrip req = do
    chan <- client_init dest
    putStrLn $ descrip ++ "Sending: " ++ show req ++ "\n"
    send chan $ req
    return chan

receiveM :: (Binary a, Show a) => String -> LibXenVChan -> IO a
receiveM descrip chan = do
  ctrlWait chan
  res <- receive chan
  putStrLn $ descrip ++ "Received: " ++ show res ++ "\n"
  return res


process :: (Binary a, Show a, Binary b, Show b) => (LibXenVChan -> IO a) 
                -> (LibXenVChan -> b -> IO ()) -> (a -> IO b) -> PlatformID -> IO ()
process recA sendB mk pId = do
  --ctrlWait chan
  chan <- server_init pId
  process' recA sendB mk chan
  close chan
  return ()

process' :: (Binary a, Show a, Binary b, Show b) => (LibXenVChan -> IO a) 
                -> (LibXenVChan -> b -> IO ()) -> (a -> IO b) -> LibXenVChan -> IO ()
process' recA sendB mk chan = do
  --ctrlWait chan
  --chan <- server_init pId
  req <- recA chan
  putStrLn "\n\nPROCESS: Request received\n\n"
  resp <- mk req
  putStrLn "\n\nPROCESS: Response constructed\n\n"
  sendB chan resp
  putStrLn "\n\nPROCESS: Response sent\n\n"
  --close chan
  return ()
