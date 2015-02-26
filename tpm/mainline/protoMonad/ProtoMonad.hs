module ProtoMonad where

import ProtoTypes
import VChanUtil

import Prelude hiding (lookup)
import Data.Map
import qualified Control.Monad.Trans.Reader as T
import Control.Monad.Error --import Control.Monad.Except
import qualified Control.Monad.Trans.Error as ET

type Proto = T.ReaderT ProtoEnv (ErrorT String IO)

runProto :: (Proto a) -> ProtoEnv ->  IO (Either String a)
runProto proto env = ET.runErrorT $ T.runReaderT proto env


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

getEntityChannel :: EntityId -> Proto LibXenVChan
getEntityChannel id = do
  eInfo <- getEntityInfo id
  return $ vChan eInfo
  
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
