-------------------------------------------------------------------------------
-- This module provides functions which perform all of the commands
-- listed in section 10 of the document: TPM Main: Part 3 - Commands
-------------------------------------------------------------------------------
module TPM.Storage where
import TPM.Const
import TPM.Driver
import TPM.Types
import TPM.Nonce
import TPM.Utils
import TPM.Digest
import TPM.Cipher
import TPM.PCR
import Data.Word
import Data.Bits(rotate, (.&.), bit)
import Data.Char(ord)
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Lazy hiding (putStrLn)
import qualified Data.ByteString.Lazy.Char8 as CHAR(pack)
import Data.Digest.Pure.SHA (hmacSha1,bytestringDigest, sha1)
import Codec.Crypto.RSA

import Prelude hiding (concat,length,map,splitAt,replicate)
import qualified Prelude as P

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
tpm_seal :: TPM tpm => tpm -> Session -> TPM_KEY_HANDLE -> TPM_DIGEST ->
                       TPM_PCR_INFO -> ByteString -> IO TPM_STORED_DATA
tpm_seal tpm (OSAP ah osn en esn scr) key pass pcr ud = do
    (rtag,size,resl,dat) <- tpm_transmit' tpm tag cod dat
    return (decode dat)
    where tag = tpm_tag_rqu_auth1_command
          cod = tpm_ord_seal
          kah = tpm_encauth_info scr en pass
          pcl = encode ((fromIntegral $ length pcb) :: UINT32)
          pcb = encode pcr
          udl = ((fromIntegral $ length ud) :: UINT32)
          dat = concat [ encode key, encode kah, pcl, pcb
                       , encode udl, ud, ah, encode osn
                       , encode False, encode ath ]
          ath = tpm_auth_hmac scr en osn 0 $ concat [ encode cod, encode kah
                                                    , pcl, pcb
                                                    , encode udl, ud ]

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
tpm_unseal :: TPM tpm => tpm -> Session -> Session -> TPM_KEY_HANDLE -> 
                         TPM_STORED_DATA -> TPM_DIGEST -> TPM_DIGEST ->
                         IO ByteString
tpm_unseal tpm (OIAP pah pen) (OIAP dah den) key sdat ppass dpass = do
    on <- nonce_create
    (rtag,size,resl,dat) <- tpm_transmit' tpm tag cod (dat on)
    let (size,dat') = splitAt 4 dat
    let size' = ((decode size) :: UINT32)
    let (dat'',_) = splitAt (fromIntegral size') dat'
    return dat''
    where tag = tpm_tag_rqu_auth2_command
          cod = tpm_ord_unseal
          dat on = concat [ encode key, encode sdat, pah, encode on
                          , encode False, encode (path on), dah
                          , encode on, encode False, encode (dath on) ]
          dath on = tpm_auth_hmac dpass den on 0 $ concat [ encode cod
                                                          , encode sdat ]
          path on = tpm_auth_hmac ppass pen on 0 $ concat [ encode cod
                                                          , encode sdat ]

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
tpm_unbind = undefined

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
tpm_createwrapkey :: TPM tpm => tpm -> Session -> TPM_KEY_HANDLE -> 
                                TPM_DIGEST -> TPM_DIGEST -> TPM_KEY -> 
                                IO TPM_KEY
tpm_createwrapkey tpm (OSAP ah osn en esn scr) parent use mig key = do
    (rtag,size,resl,dat) <- tpm_transmit' tpm tag cod (dat osn)
    return $ decode dat
    where tag = tpm_tag_rqu_auth1_command
          cod = tpm_ord_createwrapkey
          uah = tpm_encauth_info scr en use
          mah = tpm_encauth_info scr osn mig
          dat on = concat [ encode parent, encode uah, encode mah, encode key
                          , ah, encode on, encode False, encode (ath on) ]
          ath on = tpm_auth_hmac scr en on 0 $ concat [ encode cod, encode uah
                                                      , encode mah, encode key ]

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
tpm_loadkey2 :: TPM tpm => tpm -> Session -> TPM_KEY_HANDLE -> TPM_KEY -> 
                           TPM_DIGEST -> IO TPM_KEY_HANDLE
tpm_loadkey2 tpm (OIAP ah en) parent key pass = do
    on <- nonce_create
    (rtag,size,resl,dat) <- tpm_transmit tpm 45 tag cod (dat on)
    let (handle,dat') = splitAt 4 dat
    return $ decode handle
    where tag = tpm_tag_rqu_auth1_command
          cod = tpm_ord_loadkey2
          dat on = concat [ encode parent, encode key, ah, encode on
                          , encode False, encode (ath on) ]
          ath on = tpm_auth_hmac pass en on 0 $ concat [encode cod, encode key]

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
tpm_getpubkey :: TPM tpm => tpm -> Session -> TPM_KEY_HANDLE -> 
                            TPM_DIGEST -> IO TPM_PUBKEY
tpm_getpubkey tpm (OIAP ah en) key pass = do
    on <- nonce_create
    (rtag,size,resl,dat) <- tpm_transmit' tpm tag cod (dat on)
    return $ decode dat
    where tag = tpm_tag_rqu_auth1_command
          cod = tpm_ord_getpubkey
          dat on = concat [ encode key, ah, encode on
                          , encode False, encode (ath on) ]
          ath on = tpm_auth_hmac pass en on 0 (encode cod)

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

tpm_quote :: TPM tpm => tpm -> Session -> TPM_KEY_HANDLE -> TPM_NONCE ->
                        TPM_PCR_SELECTION -> TPM_DIGEST ->
                        IO (TPM_PCR_COMPOSITE, ByteString)
tpm_quote tpm shn@(OIAP ah en) key nonce pcrs pass = do
  on <- nonce_create
  (rtag,size,resl,dat) <- tpm_transmit' tpm tag cod (dat on)
  let numPcrs = P.length $ tpm_pcr_unselection pcrs
      pcb = encode pcrs
      selectionSize =  (fromIntegral $ length pcb) :: Int
      pcrsSize = numPcrs * 20
      vSize = 4 
      compositeSize = selectionSize + vSize + pcrsSize
      (comp, rest) = splitAt (fromIntegral compositeSize)  dat
      compDecoded = decode comp
      (sigSize, dat') = splitAt 4 rest
      sigSizeDecoded = ((decode sigSize) :: UINT32)
      (sig, rest2) = splitAt (fromIntegral sigSizeDecoded) dat'
      sigDecoded = decode sig
  putStrLn $ "Sig length: " ++ (show $ length sig)    
  putStrLn $ "Size of Output after sig: " ++ (show $ length rest2)
  
  return (compDecoded, sigDecoded )
  where tag = tpm_tag_rqu_auth1_command
        cod = tpm_ord_quote
        dat on = concat [ encode key, encode nonce, encode pcrs, ah,
                          encode on, encode False, encode (ath on) ]
        ath on = tpm_auth_hmac pass en on 0 $ concat [ encode cod, encode nonce,
                                                       encode pcrs]
  


-- (OSAP ah osn en esn scr)
tpm_makeidentity :: TPM tpm => tpm -> Session -> Session -> TPM_KEY ->
                               TPM_DIGEST -> TPM_DIGEST -> TPM_DIGEST ->
                               IO TPM_KEY

tpm_makeidentity tpm (OIAP sah sen) (OSAP oah oosn oen oesn oscr) key
                 spass opass ipass = do
  son <- nonce_create
  putStrLn "BEFORE"
  (rtag,size,resl,dat) <- tpm_transmit' tpm tag cod (dat son)
  putStrLn "hello"
  let newKey :: TPM_KEY
      newKey = runGet (get :: Get TPM_KEY) dat

  return $ newKey

 where tag = tpm_tag_rqu_auth2_command
       cod = tpm_ord_makeidentity
       privCA = TPM_DIGEST $ Data.ByteString.Lazy.replicate 20 ((bit 1)::Word8)
       dat son = concat [ encode kah, encode privCA, encode key, sah,
                              encode son, encode False, encode(sath son),
                              oah, encode oosn,encode False, encode(oath oosn)]
       kah = tpm_encauth_info oscr oen ipass
       sath on = tpm_auth_hmac spass sen on 0 $
                               concat [ encode cod, encode kah, encode privCA,
                                        encode key]
       oath on = tpm_auth_hmac oscr oen on 0 $
                               concat [ encode cod, encode kah, encode privCA,
                                        encode key]
                         





  
tpm_sealx = undefined




