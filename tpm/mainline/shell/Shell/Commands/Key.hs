{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE UndecidableInstances#-}
module Shell.Commands.Key (initialize,allcmds) where
import Shell.API
import Plugins.API
import System.FilePath
import Control.Monad
import Control.Monad.Trans
import System.Directory
import Data.List hiding (find)
import Data.Map hiding (map,null,filter,(\\))
import Data.Maybe
import Control.Exception
import Data.Word
import TPM
import Data.Binary

{-
dowrap s = do
    tpm <- getTPM
    shn <- getSession
    when (isNothing shn) $ do
        liftIO $ throwTPM "There must be an open OSAP session"
    let shn' = fromJust shn
    let kh = tpm_kh_srk
    res <- liftIO $ tpm_key_create_wrap tpm shn' kh Nothing Nothing Nothing
    shellPutStrLn $ "Result: " ++ show res
    return ()
-}


-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
{-
cmd_key :: (TPM t) => ShellCmd (State t)
cmd_key = ShellCmd ["key","k"]
                    "Perform key commands on the TPM"
                    "key command"
                    (withTPM dokey)
    where dokey s tpm = do
            key <- parse s
            key tpm
          enkey _ tpm = do
            tpm <- getTPM
            (pub,hash) <- liftIO $ tpm_key_pubek tpm
            shellPutStrLn $ "TPM Public Key:\n" ++ 
                            "----------------------------------------------" ++
                            "\n" ++ (show pub)
          parse s = liftIO $ witheof s use $ choice [enkp]
          enkp = command ["endorsement","ek"] none enkey
          use  = "Usage: key <endorsement|ek>"
-}

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
initialize :: (TPM t) => ShellMonad (State t) [ShellCmd (State t)]
initialize = return allcmds

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
allcmds :: (TPM t) => [ShellCmd (State t)]
allcmds = []
