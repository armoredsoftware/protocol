{-# LANGUAGE TypeSynonymInstances #-}

module ProtoTypes where

import Data.ByteString.Lazy
import Data.Binary

--Abstract entity identifier.  The id assignments are LOCAL to the particular protocol being represented.
type EntityId = Int

type Nonce = ByteString

--Common data that is sent or received by an armored entity.
data ArmoredData =
  ANonce Nonce
  | AEntityId EntityId
  | AEntityInfo EntityInfo
  | ACipherText CipherText
--TODO:  Should the following "Command" items be message items(ArmoredData) that must be evaluated in the monad prior to sending?  For now, they are implemented as seperate explicit monadic function calls.
{-| GenNonce
  | Encrypt [ArmoredData]
  | Decrypt CipherText -} 

--Binary instance necessary for communication, encryption, and decryption(or a combination of these).
instance Binary ArmoredData where
  put (ANonce n) = do put (0::Word8)
                      put n
  put (AEntityId id) = do put (1::Word8)
                          put id
  put (ACipherText ct) = do put(2::Word8)
                            put ct
  put (AEntityInfo (EntityInfo name ip)) = do put(3::Word8)
                                              put name
                                              put ip

  get = do t <- get :: Get Word8
           case t of
             0 -> do n <- get
                     return $ ANonce n
             1 -> do id <- get
                     return $ AEntityId id
             2 -> do ct <- get
                     return $ ACipherText ct
             3 -> do name <- get
                     ip <- get
                     return $ AEntityInfo $ EntityInfo name ip


type Message = [ArmoredData]
  
--This shoud contain the concrete info that is necessary to communicate with an entity
data EntityInfo = EntityInfo {
  entityName :: String,
  entityIp :: Int
} deriving (Eq, Show)

type PrivateKey = ByteString;
type PublicKey = ByteString;

--Encrypted text
type CipherText = ByteString;

type Signature = ByteString;
data SignedData a = SignedData {
  dat :: a,
  sig :: Signature
}





