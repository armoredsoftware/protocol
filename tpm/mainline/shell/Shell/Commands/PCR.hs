{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE UndecidableInstances#-}
module Shell.Commands.PCR (initialize,allcmds) where
import Shell.API
import Plugins.API
import System.FilePath
import Control.Monad
import Control.Monad.Trans
import System.Directory
import Data.List hiding (find)
import Data.Map hiding (map,null,filter,(\\))
import Data.Word
import TPM

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
cmd_pcrs :: (TPM t) => ShellCmd (State t)
cmd_pcrs = ShellCmd ["pcrs", "pcr", "p"]
                    "Execute a PCR command on the TPM"
                    "pcr command"
                    (withTPM dopcrs)
    where dopcrs s tpm = do
            pcrs <- parse s
            pcrs tpm
          readit tpm num = do val <- tpm_pcr_read tpm (fromIntegral num)
                              shellPutStrLn $ "PCR " ++ (doshow num) ++ ": " ++
                                                        (show val)
          doshow x | x < 10 = "0" ++ (show x)
          doshow x = show x
          readall _ tpm = do
            pcrs <- liftIO $ tpm_getcap_pcrs tpm
            liftIO $ mapM_ (readit tpm) [0..pcrs-1]
            return ()
          read num tpm = do
            pcr <- liftIO $ tpm_pcr_read tpm (fromIntegral num)
            shellPutStrLn $ show pcr
          reset num tpm = do
            tot <- liftIO $ tpm_getcap_pcrs tpm
            liftIO $ tpm_pcr_reset tpm (fromIntegral tot) [fromIntegral num]
            pcr <- liftIO $ tpm_pcr_read tpm (fromIntegral num)
            shellPutStrLn $ show pcr
          extend (num,pass) tpm = do
            pcr <- liftIO $ tpm_pcr_extend_with tpm (fromIntegral num) pass
            shellPutStrLn $ show pcr
          parse s = liftIO $ witheof s use $ choice [allp,redp,rstp,extp]
          allp = command ["all","a"] none readall
          redp = command ["read","rd"] integer read
          rstp = command ["reset","rst"] integer reset
          extp = command ["extend","ex"] int_and_pass extend
          use  = "Usage: pcrs <all|a>\n" ++
                 "       pcrs <read|rd> <pcr number>\n" ++
                 "       pcrs <extend|ex> <pcr number> <password>\n" ++
                 "       pcrs <reset|rst> <pcr number>"

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
initialize :: (TPM t) => ShellMonad (State t) [ShellCmd (State t)]
initialize = return allcmds

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
allcmds :: (TPM t) => [ShellCmd (State t)]
allcmds = [ cmd_pcrs ]
