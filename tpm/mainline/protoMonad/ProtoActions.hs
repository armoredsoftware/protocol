module ProtoActions where

import ProtoTypes
import ProtoMonad
import VChanUtil

import Data.ByteString.Lazy hiding (pack, map, putStrLn)
import qualified Control.Monad.Trans.Reader as T
import Data.Monoid
import Data.Binary
import qualified Codec.Crypto.RSA as C
import System.Random
import Control.Monad.IO.Class

generateNonce :: Proto Nonce
generateNonce = do
  return 56

--Encrypt with the PublicKey associated with targetId
--TODO:  Is there only 1 public key associated with each target(maybe abstractly?)
encrypt :: EntityId -> [ArmoredData]-> Proto CipherText
encrypt targetId inData = do
  pubKey <- getEntityPubKey targetId
  return $ genEncrypt pubKey inData

--Decrypt using MY PrivateKey
decrypt :: CipherText -> Proto [ArmoredData]
decrypt cipherText = do
  priKey <- T.asks (myPriKey)
  return $ genDecrypt priKey cipherText

--Sign with MY PrivateKey
sign :: [ArmoredData] -> Proto (SignedData [ArmoredData])
sign inData = do
  priKey <- T.asks (myPriKey)
  return $ genSign priKey inData

send :: EntityId -> Message -> Proto ()
send toId ds = do
  chan <- getEntityChannel toId
  logger <- liftIO createLogger
  liftIO $ sendChunkedMessageByteString logger chan (toStrict $ encode ds)
  liftIO $ putStrLn $ "Sending: " ++ (show ds)
  liftIO $ putStrLn "Sent message! " 
  return ()
  
receive :: EntityId -> Proto Message
receive fromId = do
  chan <- getEntityChannel fromId
  logger <- liftIO createLogger
  bytes <- liftIO $ readChunkedMessageByteString logger chan
  let result = decode $ fromStrict bytes
  liftIO $ putStrLn $ "Received: " ++ (show result)
  return $ result

--TODO:  Should this be in the Proto monad?(i.e. to choose packImpl).
genEncrypt :: Binary a => PublicKey -> [a] -> CipherText
genEncrypt pubKey inData = realEncrypt pubKey clearText
 where
   clearText = packImpl inData --extract packImpl from ProtoEnv here(via Proto monad)?

--TODO:  Should this be in the Proto monad?(i.e. to choose unpackImpl).
genDecrypt :: PrivateKey -> CipherText -> [ArmoredData]
genDecrypt priKey blob =
  let decrypted = realDecrypt priKey blob in
  unpackImpl decrypted

--TODO:  In Proto monad?
genSign :: Binary a => PrivateKey -> [a] -> (SignedData [a])
genSign priKey inData =
  let bytes = packImpl inData
      signature = realSign priKey bytes in
  SignedData inData signature


--Concrete implementations-------------------------------------------------
realEncrypt :: PublicKey -> ByteString -> CipherText
realEncrypt pubKey clearText = --Concrete implementation plugs in here
  let gen = mkStdGen 3
      (cipher, _) = C.encrypt gen pubKey clearText in
  cipher

realDecrypt :: PrivateKey -> CipherText -> ByteString
realDecrypt priKey cipherText = --Concrete implementation here
  C.decrypt priKey cipherText

realSign :: PrivateKey -> ByteString -> Signature --paramaterize over hash?
realSign priKey bytes = bytes --Concrete implementation plugs in here

--Concrete packing(well-defined strategy for combining elements in preparation for encryption/signing) implementation
packImpl :: (Binary a) => [a] -> ByteString
packImpl as = encode as --mconcat bslist
 --where bslist = map tobs as

--Concrete unpacking implementation
unpackImpl :: Binary a => ByteString -> [a]
unpackImpl bs = decode bs
