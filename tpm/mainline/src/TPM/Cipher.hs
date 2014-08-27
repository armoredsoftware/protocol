{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module TPM.Cipher where
import TPM.Key
import TPM.Const
import TPM.Driver
import TPM.Types
import TPM.Utils
import TPM.Nonce
import Data.Binary
import Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as CHAR
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Codec.Crypto.RSA
import Control.Exception
import Control.Monad
import Data.Maybe
import Data.Bits
import Data.Digest.Pure.SHA (Digest(..),bytestringDigest,sha1)
import Prelude (IO(..),($),(==),error)
import qualified Prelude as P

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
class TPMEncryptable a where
    tobs :: a -> ByteString
    frombs :: ByteString -> a

instance TPMEncryptable P.String where
    tobs s = CHAR.pack s
    frombs s = CHAR.unpack s

instance TPMEncryptable ByteString where
    tobs s = s
    frombs s = s

instance TPMEncryptable (Maybe TPM_DIGEST) where
    tobs Nothing = replicate 20 0
    tobs (Just (TPM_DIGEST s)) = s
    frombs s = Just (TPM_DIGEST s)

instance TPMEncryptable TPM_DIGEST where
    tobs (TPM_DIGEST bs) = bs
    frombs bs = TPM_DIGEST bs

tpm_rsa_pubencrypt :: TPMEncryptable enc => TPM_PUBKEY -> enc -> enc
tpm_rsa_pubencrypt key dat = frombs ecbs
    where size = (tpm_key_pubsize key) `P.div` 8
          expn = bs2int $ tpm_key_pubexp key
          modl = bs2int $ tpm_key_pubmod key
          rsa  = PublicKey (P.fromIntegral size) modl expn
          hf   = hashFunction ha_SHA1
          mgf  = generate_MGF1 hf
          labl = CHAR.pack "TCPA"
          ecbs = rsaes_oaep_encrypt hf mgf rsa 0 labl (tobs dat)


tpm_get_rsa_PublicKey :: TPM_PUBKEY -> IO PublicKey
tpm_get_rsa_PublicKey key = do --P.putStrLn (bshex $ tpm_key_pubexp key)
                               return $ PublicKey size modl expn
  where size = P.fromIntegral $ (tpm_key_pubsize key) `P.div` 8
        expn = bs2int $ tpm_key_pubexp key
        modl = bs2int $ tpm_key_pubmod key
         
