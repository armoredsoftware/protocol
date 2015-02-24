module ProtoActions where

import ProtoTypes
import ProtoMonad

import Data.ByteString.Lazy hiding (pack, map)
import qualified Control.Monad.Trans.Reader as T
import Data.Monoid
import Data.Binary

generateNonce :: Proto Nonce
generateNonce = do
  return empty

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
send toId ds = undefined

receive :: EntityId -> Proto Message
receive fromId = undefined

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
realEncrypt pubKey clearText = clearText --Concrete implementation plugs in here

realDecrypt :: PrivateKey -> CipherText -> ByteString
realDecrypt priKey cipherText = cipherText --Concrete implementation here

realSign :: PrivateKey -> ByteString -> Signature --paramaterize over hash?
realSign priKey bytes = bytes --Concrete implementation plugs in here

--Concrete packing(well-defined strategy for combining elements in preparation for encryption/signing) implementation
packImpl :: (Binary a) => [a] -> ByteString
packImpl as = encode as --mconcat bslist
 --where bslist = map tobs as

--Concrete unpacking implementation
unpackImpl :: Binary a => ByteString -> [a]
unpackImpl bs = decode bs
