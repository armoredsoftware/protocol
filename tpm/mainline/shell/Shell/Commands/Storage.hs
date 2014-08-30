{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE UndecidableInstances#-}
module Shell.Commands.Storage (initialize,allcmds) where
import Shell.API hiding (name)
import Plugins.API
import System.FilePath
import Data.Maybe
import Control.Exception (catch,IOException(..))
import Control.Monad
import Control.Monad.Trans
import System.Directory
import Data.List hiding (find)
import Data.Map hiding (map,null,filter,(\\))
import Data.Word
import Data.Digest.Pure.SHA (hmacSha1,bytestringDigest, sha1)
import Data.ByteString.Lazy hiding (putStrLn, map, elem, zip)
import Data.Binary
import Codec.Crypto.RSA
import Prelude hiding (catch)
import qualified Data.ByteString.Lazy.Char8 as CHAR
import TPM.SignTest

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
cmd_key :: (TPM t) => ShellCmd (State t)
cmd_key = ShellCmd ["key","k"]
                    "Perform key commands on the TPM"
                    "key command"
                    (withTPM dokey)
    where dokey s tpm = do
            key <- parse s
            key tpm 
          enkey _ tpm = do
            (pub,hash) <- liftIO $ tpm_key_pubek tpm
            shellPutStrLn $ "TPM Public Key:\n" ++  
                            "----------------------------------------------" ++
                            "\n" ++ (show pub)
          load _ tpm = do
            name <- readKeyName "Key Name: " True
            key  <- getKey name
            prnt <- readKeyHandle "Parent Key: "
            pass <- readPass "Parent Key Password: "
            (shn,clo) <- retrieveOIAP tpm
            handle <- liftIO $ tpm_loadkey2 tpm shn prnt key pass
            putLoaded name handle
            closeSession tpm clo shn
            shellPutStrLn $ "Key loaded successfully"
          create _ tpm = do
            name  <- readKeyName "Key Name: " False
            parnt <- readKeyHandle "Parent Key: "
            npass <- readPass "Key Password: "
            ppass <- readPass "Parent Key Password: "
            let key = tpm_key_create tpm_auth_priv_use_only
                keySize = (fromIntegral $ Data.ByteString.Lazy.length $ encode key):: Int
            let kty = tpm_et_xor_keyhandle
            shn <- liftIO $ tpm_session_osap tpm ppass kty parnt
            key' <- liftIO $ tpm_createwrapkey tpm shn parnt npass npass key
            let key'Size = (fromIntegral $ Data.ByteString.Lazy.length $ encode key'):: Int
            closeSession tpm True shn
            putKey name key'
            shellPutStrLn $ "Key " ++ name ++ " created\n" ++ show key'
            shellPutStrLn $ "Key size BEFORE: " ++ (show keySize)
            shellPutStrLn $ "Key size AFTER: " ++ (show key'Size)
            
          createSign _ tpm = do
            name  <- readKeyName "Key Name: " False
            parnt <- readKeyHandle "Parent Key: "
            npass <- readPass "Key Password: "
            ppass <- readPass "Parent Key Password: "
            let key = tpm_key_create_signing tpm_auth_always
            shellPutStrLn $ show key
            let kty = tpm_et_xor_keyhandle
            shn <- liftIO $ tpm_session_osap tpm ppass kty parnt
            key' <- liftIO $ tpm_createwrapkey tpm shn parnt npass npass key
            closeSession tpm True shn
            putKey name key'
            shellPutStrLn $ "Key " ++ name ++ " created\n" ++ show key'

          createIdentity _ tpm = do
            spass <- readPass "SRK Password: "
            opass <- readPass "Owner Password: "
            name  <- readKeyName "Key Name: " False
            ipass <- readPass "Key Password: "
            let key = tpm_key_create_identity tpm_auth_priv_use_only
                keySize = (fromIntegral $ Data.ByteString.Lazy.length $ encode key):: Int
                kty = tpm_et_xor_owner
            (sShn,clo) <- retrieveOIAP tpm
            oShn <- liftIO $ tpm_session_osap tpm opass kty (0x40000001 :: Word32)
            
            key' <- liftIO $ tpm_makeidentity tpm sShn oShn key
                                              spass opass ipass
            
            let key'Size = (fromIntegral $ Data.ByteString.Lazy.length $ encode key'):: Int
            --closeSession tpm clo sShn
            --closeSession tpm True oShn
            

            putKey name key'
            shellPutStrLn $ "Identity Key " ++ name ++ " created\n" ++ (show key')
            shellPutStrLn $ "Key size BEFORE: " ++ (show keySize)
            shellPutStrLn $ "Key Before: " ++ (show key)
            shellPutStrLn $ "Key size AFTER: " ++ (show key'Size)
            shellPutStrLn $ "Key After: " ++ (show key')
          
          evict name tpm = do
            handle <- getLoaded name
            liftIO $ tpm_flushspecific tpm handle tpm_rt_key
            removeLoaded name
            shellPutStrLn $ "Key evicted successfully"
          destroy name tpm = do
            removeKey name
            shellPutStrLn $ "Destroyed key " ++ name ++ " successfully"
          getpub _ tpm = do
            key  <- readKeyHandle "Key: "
            pass <- readPass "Password: "
            (shn,clo) <- retrieveOIAP tpm
            pub <- liftIO $ tpm_getpubkey tpm shn key pass
            closeSession tpm clo shn
            shellPutStrLn $ "Public Key:\n" ++ 
                            "--------------------------------------------\n" ++
                            show pub

          quote name tpm = do
            handle <- getLoaded name
            pass <- readPass "Key Password: "
            max   <- liftIO $ tpm_getcap_pcrs tpm
            let list = [0..23] :: [Word8]
                pcrSelect = tpm_pcr_selection max list
            (shn,clo) <- retrieveOIAP tpm
            nonce <- liftIO $ nonce_create
            compGolden <- liftIO $ tpm_pcr_composite tpm pcrSelect
            (comp,sig) <- liftIO $ tpm_quote tpm shn handle nonce pcrSelect pass
            closeSession tpm clo shn
            --compGolden <- liftIO $ tpm_pcr_composite tpm pcrSelect
            {-liftIO $ putStrLn $ "Golden comp length: " ++
                                 (show $ Data.ByteString.Lazy.length (encode compGolden)) -}
            
            let x :: TPM_QUOTE_INFO
                x = TPM_QUOTE_INFO (tpm_pcr_composite_hash $ compGolden) nonce

            --liftIO $ putStrLn $ mkhex tpm_quote_info_fixed

            let blob :: ByteString
                blob = {- bytestringDigest $ sha1 $ -}encode $ tpm_digest $ encode x

            (shn2, clo2) <- retrieveOIAP tpm
            pubKey <- liftIO $ tpm_getpubkey tpm shn2 handle pass
            publicKey <- liftIO $ tpm_get_rsa_PublicKey pubKey
            --liftIO $ putStrLn (show publicKey)
            {-case (rsassa_pkcs1_v1_5_verify ha_SHA1 publicKey blob sig) of
              True -> liftIO $ putStrLn "Verified"
              False -> liftIO $ putStrLn "NOT Verified" -}


            let (pub, pri) = getKeyPair
            liftIO $ putStrLn (show pub)
            liftIO $ putStrLn (show pri)
            
            qi <- liftIO $ makeQuoteInfo tpm
            let qiBlob = encode (blob)
                qiSig = rsassa_pkcs1_v1_5_sign ha_SHA1 pri qiBlob
            liftIO $ putStrLn "Signed..."
            liftIO $ putStrLn $ "sigLength: " ++ (show $ Data.ByteString.Lazy.length qiSig)
            case rsassa_pkcs1_v1_5_verify ha_SHA1 pub qiBlob qiSig of
              True -> liftIO $ putStrLn "VERIFIED!!!"
              False -> liftIO $ putStrLn "NOT Verified!!!!"


            let pcrs = tpmPcrCompositePcrs comp
                output = zip list pcrs
                doshow x | x < 10 = "0" ++ (show x)
                doshow x = show x
                
                f (x, y) = shellPutStrLn $ "PCR " ++ (doshow x) ++ ": " ++ (show y)
            --liftIO $ putStrLn "Quoted PCRS: "
           -- liftIO $ mapM_ f output
            let gPcrs = tpmPcrCompositePcrs compGolden
                gOutput = zip list gPcrs

           -- liftIO $ putStrLn "Golden PCRS: "
            --liftIO $ mapM_ f gOutput

            {-
            case compGolden == comp of
              True -> liftIO $ putStrLn "Same"
              False -> liftIO $ putStrLn "Different" -}

            closeSession tpm clo shn


            



            
            

            {-
            readit tpm num = do val <- tpm_pcr_read tpm (fromIntegral num)
                              shellPutStrLn $ "PCR " ++ (doshow num) ++ ": " ++
                                                        (show val)
            doshow x | x < 10 = "0" ++ (show x)
            doshow x = show x
            readall _ tpm = do
            pcrs <- liftIO $ tpm_getcap_pcrs tpm
            liftIO $ mapM_ (readit tpm) [0..pcrs-1]
            return ()
            -}
            
            
            shellPutStrLn $ "Quote Completed"
          
          list _ tpm = do
            keys <- getAllKeys
            loaded <- getAllLoaded
            let l = map fst loaded
            shellPutStrLn $ "Keys: " ++ (unwords (map (msg l) keys))
            where msg l (k,_) | k `elem` l = "[" ++ k ++ "]"
                  msg l (k,_) = k
          parse s = liftIO $ witheof s use $ choice [enkp,loadp,cretp,cresp,
                                                     creip,evicp,lstp,destp,
                                                     gpubp, quotp]
          lstp  = command ["list","l"] none list
          enkp  = command ["endorsement","ek"] none enkey
          loadp = command ["load","ld"] none load
          cretp = command ["create","cr"] none create
          cresp = command ["createSign", "cs"] none createSign
          creip = command ["createIdentity", "ci"] none createIdentity
          evicp = command ["evict","ev"] name evict
          destp = command ["destroy","ds"] name destroy
          gpubp = command ["getpub","gp"] none getpub
          quotp = command ["quote", "q"] name quote
          use  = "Usage: key <list,l>\n" ++
                 "       key <endorsement|ek>\n" ++
                 "       key <create|cr>\n" ++
                 "       key <createSign|cs>\n" ++
                 "       key <createIdent|ci>\n" ++
                 "       key <load|ld>\n" ++
                 "       key <getpub|gp>\n" ++
                 "       key <destroy|ds> <name>\n" ++
                 "       key <evict|ev> <name>\n" ++
                 "       key <quote|q> <name>"
                 

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
cmd_store :: (TPM t) => ShellCmd (State t)
cmd_store = ShellCmd ["store","st"]
                    "Perform storage commands on the TPM"
                    "store command"
                    (withTPM dostore)
    where dostore s tpm = do
            store <- parse s
            store tpm 
          seal _ tpm = do
            max   <- liftIO $ tpm_getcap_pcrs tpm
            name  <- shellGetLine "Name: "
            prnt  <- readKeyHandle "Key: "
            dpass <- readPass "Data Password: "
            ppass <- readPass "Key Password: "
            spcrs <- readPCRSelect max "PCR Selection: "
            udata <- shellGetPassLn "Sealed Data: "
            scomp <- liftIO $ tpm_pcr_composite tpm spcrs
            let kty = tpm_et_xor_keyhandle
            let udata' = CHAR.pack udata
            let phash = tpm_pcr_composite_hash scomp
            let pinfo = TPM_PCR_INFO spcrs phash phash
            shn <- liftIO $ tpm_session_osap tpm ppass kty prnt
            dat <- liftIO $ tpm_seal tpm shn prnt dpass pinfo udata'
            putSealed name dat
            shellPutStrLn $ show dat
          unseal _ tpm = do
            name  <- shellGetLine "Name: "
            prnt  <- readKeyHandle "Key: "
            dpass <- readPass "Password: "
            ppass <- readPass "Key Password: "
            sdat  <- getSealed name
            ksh <- liftIO $ tpm_session_oiap tpm
            dsh <- liftIO $ tpm_session_oiap tpm
            dat <- liftIO $ tpm_unseal tpm ksh dsh prnt sdat ppass dpass
            shellPutStrLn $ "Unsealed data: " ++ (CHAR.unpack dat)
          destroy name tpm = do
            removeSealed name
            shellPutStrLn $ "Destroyed stored data " ++ name ++ " successfully"
          list _ tpm = do
            sealed <- getAllSealed
            shellPutStrLn $ "Store: " ++ (unwords (map fst sealed))
          parse s = liftIO $ witheof s use $ choice [sealp,uselp,lstp,destp]
          lstp  = command ["list","l"] none list
          sealp = command ["seal","sl"] none seal
          uselp = command ["unseal","us"] none unseal
          destp = command ["destroy","ds"] name destroy
          use  = "Usage: store <list,l>\n" ++
                 "       store <destory|ds> <name>\n" ++
                 "       store <seal|sl> <name> <key> <pass> <data>\n" ++
                 "       store <unseal|us> <name> <key> <key pass> <data pass>"
            
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
initialize :: (TPM t) => ShellMonad (State t) [ShellCmd (State t)]
initialize = return allcmds                         
          
-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
allcmds :: (TPM t) => [ShellCmd (State t)]
allcmds = [ cmd_key, cmd_store ]
