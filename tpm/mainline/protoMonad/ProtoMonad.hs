module ProtoMonad where

import ProtoTypes

import Prelude hiding (lookup)
import Data.Map
import qualified Control.Monad.Trans.Reader as T
import Control.Monad.Error --import Control.Monad.Except


type Proto = T.ReaderT ProtoEnv (ErrorT String IO)

data ProtoEnv = ProtoEnv {
  me :: EntityId,
  myPriKey :: PrivateKey,
  entities :: Map EntityId EntityInfo,
  publicKeys :: Map EntityId PublicKey,
  --privateKeys :: Map Int PrivateKey,
  packScheme :: Int,
  encScheme :: Int,
  signScheme :: Int
}

getEntityInfo :: EntityId -> Proto EntityInfo
getEntityInfo i = do
  infos <- T.asks entities
  let maybeInfo = lookup i infos
  case maybeInfo of
    Nothing -> throwError ("No known EntityInfo for Entity with id: "
                           ++ (show i) )
    Just info -> return info

getEntityPubKey :: EntityId -> Proto PublicKey
getEntityPubKey i = do
  pubKeys <- T.asks publicKeys
  let maybePubKey = lookup i pubKeys
  case maybePubKey of
    Nothing -> throwError ("No known PublicKey for Entity with id: "
                           ++ (show i) )
    Just pubKey -> return pubKey 
