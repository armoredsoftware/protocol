{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE UndecidableInstances#-}
module Shell.Commands.Session (initialize,allcmds) where
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
import TPM.Session
import TPM.Driver
import TPM.Types
import TPM.Const
import Data.ByteString.Lazy

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
cmd_session :: (TPM t) => ShellCmd (State t)
cmd_session = ShellCmd ["session", "sess", "s"]
                        "Open or close a TPM session"
                        "session command"
                        (withTPM dosess)
    where dosess s tpm = do
            sess <- parse s
            sess tpm
            return ()
          oiap _ tpm = do
            shn <- liftIO $ tpm_session_oiap tpm
            setSession (Just shn)
            shellPutStrLn "OIAP session opened"
          osap _ tpm = do
            let pass = tpm_digest_pass ""
            shn <- liftIO $ tpm_session_osap tpm pass tpm_et_xor_owner 0
            setSession (Just shn)
            shellPutStrLn "OSAP session opened"
          close _ tpm = do
            shn <- retrieveSession
            liftIO $ tpm_session_close tpm shn
            shellPutStrLn "Session closed"
          parse s = liftIO $ witheof s use $ choice [oiapp,osapp,closp]
          oiapp = command ["oiap","oi"] none oiap 
          osapp = command ["osap","os"] none osap
          closp = command ["close","cl"] none close
          use   = "Usage: session oiap\n" ++
                  "       session osap\n" ++
                  "       session close"

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
initialize :: (TPM t) => ShellMonad (State t) [ShellCmd (State t)]
initialize = return allcmds

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
allcmds :: (TPM t) => [ShellCmd (State t)]
allcmds = [ cmd_session ]
