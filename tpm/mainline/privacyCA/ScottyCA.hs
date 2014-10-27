{-# LANGUAGE OverloadedStrings #-}

module ScottyCA where

import Web.Scotty
import Data.ByteString.Lazy (ByteString, append, empty, pack, length, toStrict, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as L

import Data.Binary
import System.IO
import Crypto.Cipher.AES
import Control.Monad.Trans
import qualified Data.Text.Lazy as LazyText
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy.Encoding as LazyEncoding
import qualified Demo3Shared as AD  --ArmoredData
import TPM


scottyCAMain = scotty 3000 $ do
  --  get "/" $ text "foobar"
    Web.Scotty.get "/foo" $ do
      v <- param "fooparam"
      html $ mconcat ["<h1>", v, "</h1>"]
      

    post "/" $ do
  --    req <- request
     --   bod <- body
       -- a <- jsonData :: ActionM String
   --     text  a
        --json (bod :: String) --(a :: String)
      a <- (param "request") :: ActionM LazyText.Text
      
      html a
      let jj = (AD.jsonDecode (LazyEncoding.encodeUtf8 a) :: Maybe AD.CARequest)
      case jj of
	   Nothing -> text "you suck"
	   Just caReq -> do caResp <- liftIO $ handleCAReq caReq
                            json caResp
      --return ()
      --text "posted!"
   --     text (LazyText.pack (L.unpack bod))
   
handleCAReq :: AD.CARequest -> IO AD.CAResponse
handleCAReq (AD.CARequest id (AD.Signed idContents idSig)) = do
  ekPubKey <- readPubEK
  let iPubKey = identityPubKey idContents
      iDigest = tpm_digest $ encode iPubKey
      asymContents = contents iDigest
      blob = encode asymContents
      encBlob =  tpm_rsa_pubencrypt ekPubKey blob
      
      caPriKey = snd AD.generateCAKeyPair
      caCert = AD.signPack caPriKey iPubKey
      certBytes = encode caCert
      
      strictCert = toStrict certBytes
      encryptedCert = encryptCTR aes ctr strictCert
      enc = fromStrict encryptedCert
      --encryptedSignedAIK = crypt' CTR symKey symKey Encrypt signedAIK  

      --enc = encrypt key certBytes
  return (AD.CAResponse enc encBlob)
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



readPubEK :: IO TPM_PUBKEY
readPubEK = do
  handle <- openFile AD.exportEKFileName ReadMode
  pubKeyString <- hGetLine handle
  let pubKey :: TPM_PUBKEY
      pubKey = read pubKeyString
  hClose handle
  return pubKey