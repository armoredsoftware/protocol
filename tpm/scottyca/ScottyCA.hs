{-# LANGUAGE OverloadedStrings #-}

module ScottyCA where

import Web.Scotty
import Data.ByteString.Lazy (ByteString, append, empty, pack, toStrict, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as L hiding (length)

import Data.Binary
import System.IO
import Crypto.Cipher.AES
import Control.Monad.Trans
import qualified Data.Text.Lazy as LazyText
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy.Encoding as LazyEncoding
import qualified Demo3SharedNOVCHAN as AD  --ArmoredData
import TPM

import Database.HDBC 
import Database.HDBC.Sqlite3
import Control.Monad.Reader

--0= nothing
--1= basic
--2= heavy
outputlevel = 1

--type MyReader a b= ReaderT (IO a) b
scottyCAMain :: Int -> Int -> IO ()
scottyCAMain port verbosity = scotty port $ do
    Web.Scotty.get "/" $ text "foobar"  --a quick way to check if the CA is 
    				 	--serving, client should see "foobar" 
    				 	--(Note: unqualified get is ambiguous).

    post "/" $ do
      --reads in "potential" request (parsing it could fail). 
      --Note: no en/de-crypting is yet taking place.
      a <- (param "request") :: ActionM LazyText.Text  
      myprint' ("Received (Text):\n" ++ (show a)) 2 --debug show of text.
      myprint' ("Received (UTF8):\n" ++ (show (LazyEncoding.encodeUtf8 a))) 2 --debug printout.
      myprint' ("Data received on port: " ++ (show port)) 1
      
      --first converts the Text to UTF8, then then attempts to read a CARequest
      let jj = AD.jsonEitherDecode (LazyEncoding.encodeUtf8 a) :: Either String AD.CARequest
      myprint' ("JSON decoded CARequest result:\n" ++ (show jj)) 2 --debug 

      case jj of
	   (Left err)    -> do
	   		     let str = "Error decoding CARequest from JSON. Error was: " ++ err
                             myprint' str 1    --report read error: console
                             text (LazyText.pack str) --report read error: client
	   (Right caReq) -> do 
	   		     myprint' ("Successfully parsed request from JSON:\n" ++ (show caReq)) 1
                             --json :: ToJSON a => a -> ActionM ()
                             --Set the body of the response to the JSON encoding of the given value. 
                             --Also sets "Content-Type" header to "application/json; charset=utf-8" if it has not already been set.
                             json caReq  --I think we can delete this. was for early testing (echoing). -paul
                             
                             --now that we have succesfully parsed a CARequest, let us attempt to formulate a CAResponse.
                             eitherCAResp <- liftIO $ handleCAReq caReq
                             case eitherCAResp of
                                (Left err) 	-> do
                                		     let str = ("Error formulating CAResponse. Error was: " ++ err)
                                                     myprint' str 1    --report error: console
                                                     text (LazyText.pack str) --report error: client
                                (Right caResp) 	-> do
                                		     myprint' "Sending response" 1
                                                     json caResp 

   
handleCAReq :: AD.CARequest -> IO (Either String AD.CAResponse)
handleCAReq (AD.CARequest id (AD.Signed idContents idSig)) = do
  --try to find a pubkey associated with the given ID.
  eitherEKPubKey <- ekLookup id
  
  case (eitherEKPubKey) of
  	(Left err) -> do
  			let str = ("LOOKUP FAILURE. Error was: " ++ err)
			myprint str 1
  			return $ Left str
  	(Right ekPubKey) -> do
		  let iPubKey = identityPubKey idContents
		      iDigest = tpm_digest $ encode iPubKey
		      asymContents = contents iDigest
		      blob = encode asymContents
		      encBlob =  tpm_rsa_pubencrypt ekPubKey blob
		      
		      caPriKey = snd AD.generateCAKeyPair  --TODO: this needs fixing. fix as in correct and fix as in stationary
		      caCert = AD.signPack caPriKey iPubKey
		      certBytes = encode caCert
		      
		      strictCert = toStrict certBytes
		      encryptedCert = encryptCTR aes ctr strictCert
		      enc = fromStrict encryptedCert
		      --encryptedSignedAIK = crypt' CTR symKey symKey Encrypt signedAIK  

		      --enc = encrypt key certBytes
		  return (Right (AD.CAResponse enc encBlob))
 where 
   symKey = 
     TPM_SYMMETRIC_KEY 
     (tpm_alg_aes128) 
     (tpm_es_sym_ctr) 
     key
   
   v:: Word8 
   v = 1
   key = ({-B.-}Data.ByteString.Lazy.pack $ replicate 16 v) 
   --strictKey = toStrict key
   aes = initAES $ toStrict key
   ctr = toStrict key
   contents dig = TPM_ASYM_CA_CONTENTS symKey dig

databaseName = "armoredDB.db"

ekLookup :: Int -> IO (Either String TPM_PUBKEY)
ekLookup my_id = do
                myprint "beginning ekLookup..." 2 --debug
                conn <- connectSqlite3 databaseName
                res <- quickQuery' conn ("SELECT jsontpmkey from pubkeys where id =" ++ (show my_id)) []
                disconnect conn
                myprint ("Here is the result of the query:\n" ++ (show res) ) 2
                case (length res) of
                     0 -> do 
                     	   let err = "No entry found for given ID: " ++ (show my_id)
                     	   myprint err 1
                           return $ Left err
                     1 -> do
                           let res' = (res !! 0) !! 0
                               convertedRes = fromSql res' :: ByteString
                           myprint "bytestring version of tpmkey: " 2
                           myprint (show convertedRes) 2
                           let x = AD.jsonEitherDecode convertedRes :: Either String TPM_PUBKEY 
                           myprint ("One entry found for ID: " ++ (show my_id)) 1
                           myprint ("Result of JSON decoding of query:\n" ++ ( show x)) 2
                           case x of
                                 (Left err) -> do 
                                 		let str ="Failed to convert DB entry of key from JSON to Haskell type. Error was: " ++ err
                                                myprint str 1
                                                return $ Left str
                                 (Right k) -> do 
                                                myprint ("SUCCESS!-fully parsed associated key from DB: " ++ (show databaseName)) 1
                                                return $ Right k
                     x -> do
                     	    let str = ("Ambiguity error: Multiple entries found for given ID: " ++ (show my_id))
                            myprint str 1
                            return $ Left str

readPubEK :: IO TPM_PUBKEY
readPubEK = do
  handle <- openFile AD.exportEKFileName ReadMode
  pubKeyString <- hGetLine handle
  let pubKey :: TPM_PUBKEY
      pubKey = read pubKeyString
  hClose handle
  return pubKey
  
editTPM_PUBKEYentry :: Int -> TPM_PUBKEY -> IO (Either String String)
editTPM_PUBKEYentry id newtpmkey = do
				    conn <- connectSqlite3 databaseName
				    return (Left "nothing yet. TODO")

myprint' ::(Monad m) => String -> Int -> m (IO ())
myprint' str i = return (myprint str i)

myprint :: String -> Int -> IO ()
myprint msg i | i<= outputlevel = putStrLn msg
	      | otherwise 	= return ()


