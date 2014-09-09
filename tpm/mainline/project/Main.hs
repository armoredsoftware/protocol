{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main,module Plugins.API) where
import Shell
import Plugins.API
{-
import Paths_tpm
import Control.Monad
import Control.Monad.Trans
import System.Environment
import System.Directory
import System.FilePath
-}
import qualified Shell.Commands.PCR as PCR
import qualified Shell.Commands.Admin as ADM
import qualified Shell.Commands.Session as SHL
import qualified Shell.Commands.Capability as CAP
import qualified Shell.Commands.Key as KEY
import qualified Shell.Commands.Storage as STO
{-
import Prelude hiding (writeFile,readFile)
import Data.ByteString (readFile,writeFile)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Binary (encode,decode)
import Control.Exception (tryJust, catch, IOException, SomeException)
-}
import TPM
{-
import OpenSSL (withOpenSSL)
import OpenSSL.EVP.Cipher
-}
import Appraiser
import Attestation
import Measurer
import Demo3Shared

{-
cmd_log :: (TPM t) => ShellCmd (State t)
cmd_log = ShellCmd ["log", "l"]
                    "Enable or disable logging on TPM command strings"
                    "internal command"
                    docmd
    where docmd "toggle" = getLogging >>= setLogging . not >> showlog
          docmd "on" = setLogging True >> showlog
          docmd "off" = setLogging False >> showlog
          docmd "" = showlog
          docmd s = shellPutStrLn "Usage: log [on|off|info]"
          showlog = getLogging >>= \l -> shellPutStrLn ("Logging is " ++ msg l)
          msg True = "on"
          msg False = "off"

envplugs = liftIO $ (getEnv "TPM_PLUGINS") `catch` (\(_:: SomeException) -> return "")
envpaths = liftIO $ (getEnv "TPM_PATHS") `catch` (\(_:: SomeException) -> return "")

breakon c [] = []
breakon c s  = case break (== c) s of
                    (a,[])  -> [a]
                    (a,c:b) -> a:(breakon c b)

plugins = do
    env  <- liftM (breakon ':') envplugs
    home <- getHomeDirectory
    datd <- getDataFileName ""
    return (env ++ [home++"/.tpm"] ++ [datd])

paths = do
    env   <- liftM (breakon ':') envpaths
    home  <- getHomeDirectory
    datd  <- getDataFileName ""
    return (env ++ [home++"/.tpm"] ++ [datd])

history = do
    home <- getHomeDirectory
    createDirectoryIfMissing True (home </> ".tpm")
    return $ Just (home </> ".tpm" </> "history")

loadstate :: IO ([(String,TPM_KEY)], [(String,TPM_STORED_DATA)])
loadstate = do
    home <- getHomeDirectory
    createDirectoryIfMissing True (home </> ".tpm")
    let file = home </> ".tpm" </> "state"
    res <- tryJust (\(e :: IOException) -> return $ Just ()) (readFile file)
    case res of 
        Left _ -> return ([],[])
        Right r -> return $ decode (LBS.fromChunks [r])

savestate :: [(String,TPM_KEY)] -> [(String,TPM_STORED_DATA)] -> IO ()
savestate keys sealed = do
    home <- getHomeDirectory
    createDirectoryIfMissing True (home </> ".tpm")
    let file = home </> ".tpm" </> "state"
    writeFile file (BS.concat $ LBS.toChunks (encode (keys,sealed)))
    return ()

close = do
    tpm <- getTPM
    sta <- shellGetState
    case session sta of
        Nothing  -> return ()
        Just sh  -> do shellPutStr "Closing active session..." 
                       liftIO $ tpm_session_close tpm sh
                       shellPutStrLn "done"
    mapM_ (flushkey tpm) (loaded sta)
    save <- liftIO $ savestate (keys sta) (sealed sta)
    shellPutStrLn $ "Exiting TPM interpreter. Goodbye."
    where flushkey tpm (name,key) = do
            liftIO $ tpm_flushspecific tpm key tpm_rt_key
            shellPutStrLn $ "Evicted loaded key " ++ name ++ " from TPM."

shell tpm = do 
    plugpaths <- plugins
    srcpaths  <- paths
    hist      <- history
    (kys,sld) <- loadstate
    let state = initial tpm kys sld
    let shell' = initialShell state
    return $ shell' { pluginDirs = plugpaths
                    , cmdPrefix = ""
                    , prompt = "tpm> "
                    , greetMsg = Just "Welcome to TPM"
                    , defaultEval = ""
                    , cmdBuiltin = [ exitCommand ["quit","exit",":q",":e"]
                                   , helpCommand ["help","?"]
                                   , cmd_log ] ++
                                   PCR.allcmds ++
                                   ADM.allcmds ++
                                   SHL.allcmds ++
                                   CAP.allcmds ++
                                   STO.allcmds ++
                                   KEY.allcmds
                    , closeShell = close
                    , historyFile = hist
                    }


-}

main = do {-withOpenSSL $ do 
    let tpm = tpm_socket "/var/run/tpm/tpmd_socket:0"
    shell' <- shell tpm
    runShell shell' -}
    putStrLn "project main"
    return () 

