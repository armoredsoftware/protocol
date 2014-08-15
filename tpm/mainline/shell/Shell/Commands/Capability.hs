{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE UndecidableInstances#-}
module Shell.Commands.Capability (initialize,allcmds) where
import Shell.API
import Plugins.API
import System.FilePath
import Control.Monad
import Control.Monad.Trans
import System.Directory
import Data.Maybe
import Control.Exception hiding (try)
import Data.List hiding (find)
import Data.Map hiding (map,null,filter,(\\))
import Data.Word
import TPM
import Data.Binary (encode)
import Data.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as CHAR

doversion "" = do
    tpm <- getTPM
    ver <- liftIO $ tpm_getcap_version tpm
    shellPutStrLn $ "Version: " ++ (show ver)
    return ()
doversion s = do
    shellPutStrLn $ "Usage: version"
    return ()

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
cmd_info :: (TPM t) => ShellCmd (State t)
cmd_info = ShellCmd ["info", "i"]
            "Determine the TPM information"
            "capability command"
            (withTPM doinfo)
    where doinfo s tpm = do
            info <- parse s
            info tpm
            return ()
          all _ tpm = do
            vers <- liftIO $ tpm_getcap_version tpm
            pcrs <- liftIO $ tpm_getcap_pcrs tpm
            mnft <- liftIO $ tpm_getcap_mnft tpm
            auth <- liftIO $ tpm_getcap_max_authsess tpm
            tran <- liftIO $ tpm_getcap_max_transess tpm
            daa  <- liftIO $ tpm_getcap_max_daasess tpm
            cntr <- liftIO $ tpm_getcap_max_counters tpm
            keys <- liftIO $ tpm_getcap_max_keys tpm
            cnxt <- liftIO $ tpm_getcap_max_context tpm
            sess <- liftIO $ tpm_getcap_max_sessions tpm
            buff <- liftIO $ tpm_getcap_buffer tpm
            ownr <- liftIO $ tpm_getcap_owner tpm
            shellPutStrLn $ "TPM Information: v" ++ (show vers)
            shellPutStrLn $ "-------------------------------------------------"
            shellPutStrLn $ "    Number of PCRs       " ++ (show pcrs)
            shellPutStrLn $ "    Manufacturer ID:     " ++ (show mnft) ++ 
                            " (" ++ (CHAR.unpack $ encode mnft) ++ ")"
            shellPutStrLn $ "    Max. Auth. Sessions: " ++ (show auth)
            shellPutStrLn $ "    Max. Tran. Sessions: " ++ (show tran)
            shellPutStrLn $ "    Max. DAA Sessions:   " ++ (show daa)
            shellPutStrLn $ "    Max. Sessions:       " ++ (show sess)
            shellPutStrLn $ "    Number of Counters:  " ++ (show cntr)
            shellPutStrLn $ "    Number of RSA Keys:  " ++ (show keys)
            shellPutStrLn $ "    Max. Saved Sessions: " ++ (show cnxt)
            shellPutStrLn $ "    I/O Buffer Size:     " ++ (show buff)
            shellPutStrLn $ "    TPM has Owner:       " ++ (show ownr)
          version _ tpm = do
            vers <- liftIO $ tpm_getcap_version tpm
            shellPutStrLn $ "Version: " ++ (show vers)
          key _ tpm = do
            keys <- liftIO $ tpm_getcap_key_handle tpm
            shellPutStrLn $ "Loaded Keys:\n" ++ show keys
          help _ tpm = shellPutStrLn use
          parse "" = return (all ())
          parse s = liftIO $ witheof s use $ choice [allp,verp,hlpp,keyp]
          allp = command ["all"] none all
          verp = command ["version","v"] none version
          keyp = command ["keys","k"] none key
          hlpp = command ["help","?"] none help
          use = "Usage: info [all]\n" ++
                "       info <version|v>\n" ++
                "       info <keys|k>"

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
initialize :: (TPM t) => ShellMonad (State t) [ShellCmd (State t)]
initialize = return allcmds

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
allcmds :: (TPM t) => [ShellCmd (State t)]
allcmds = [ cmd_info ]
