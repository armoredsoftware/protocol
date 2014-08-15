{-# OPTIONS -fglasgow-exts #-}
{-# LANGUAGE UndecidableInstances#-}
module Shell.Commands.Admin (initialize,allcmds) where
import Shell.API
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
import Prelude hiding (catch)

-------------------------------------------------------------------------------
-- Provide a command to support testing of the TPM. This command
-- supports but the TPM full test and the TPM continue test commands.
-------------------------------------------------------------------------------
cmd_test :: TPM t => ShellCmd (State t)
cmd_test = ShellCmd ["test", "selftest", "t"]
                    "Instruct the TPM to perform internal testing"
                    "admin command"
                    (withTPM doselftest)
    where doselftest s tpm = do
            test <- parse s
            liftIO $ test tpm
            res <- liftIO $ tpm_gettestresult tpm
            shellPutStrLn $ "Test Result: " ++ (bs2str res)
          parse s = liftIO $ witheof s use $ 
                        (string "continue" >> return tpm_continueselftest) <|>
                        (option tpm_selftestfull 
                            (string "full" >> return tpm_selftestfull))
          use = "Usage: selftest [full|continue]"

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
cmd_admin :: TPM t => ShellCmd (State t)
cmd_admin = ShellCmd ["admin", "adm", "a"]
                      "Perform an administrative command on the TPM"
                      "admin command"
                      (withTPM doadmin)
    where doadmin "" tpm = shellPutStrLn use
          doadmin s tpm = do
            admin <- parse s
            admin tpm
            return ()
          savestate _ tpm = do 
            liftIO $ tpm_savestate tpm
            shellPutStrLn $ "Save state succeeded"
          setowner v tpm = do
            liftIO $ tpm_setownerinstall tpm v
            shellPutStrLn $ "Set owner install succeeded"
          tempdeact p tpm = do 
            let pass = tpm_digest_pass p
            (shn,clo) <- retrieveOIAP tpm
            liftIO $ tpm_settempdeactivated tpm shn pass
            closeSession tpm clo shn
            shellPutStrLn $ "TPM deactivated until reboot"
          disowner val p tpm = do 
            let pass = tpm_digest_pass p
            (shn,clo) <- retrieveOIAP tpm
            liftIO $ tpm_ownersetdisable tpm shn val pass
            closeSession tpm clo shn
            shellPutStrLn $ if val then dis else ena
            where dis = "TPM disabled successfully"
                  ena = "TPM enabled successfully"
          deactivate val _ tpm = do
            liftIO $ tpm_physicalsetdeactivated tpm val
            shellPutStrLn $ if val then dea else act
            where dea = "TPM deactivated successfully"
                  act = "TPM activated successfully"
          physical True tpm = do
            liftIO $ tpm_physicalenable tpm
            shellPutStrLn $ "Physical presence enabled"
          physical False tpm = do
            liftIO $ tpm_physicaldisable tpm
            shellPutStrLn $ "Physical presence disabled"
          operauth p tpm = do
            let pass = tpm_digest_pass p
            liftIO $ tpm_setoperatorauth tpm pass
            shellPutStrLn $ "Operator password set successfully"
          parse s = liftIO $ witheof s use $ choice [savestatep,setownerp
                                                    ,disownerp,enaownerp
                                                    ,physicalp,deactivep
                                                    ,activep,tempdeacp
                                                    ,operauthp]
          savestatep = command ["savestate","ss"] none savestate
          setownerp = command ["ownerinstall","oi"] bool setowner
          disownerp = command ["disable","dis"] pass (disowner True)
          enaownerp = command ["enable","ena"] pass (disowner False)
          deactivep = command ["deactivate","dv"] none (deactivate True)
          activep   = command ["activate","av"] none (deactivate False)
          physicalp = command ["physical","phy"] enable physical
          tempdeacp = command ["tempdeactivate","td"] pass tempdeact
          operauthp = command ["opauth","oa"] pass operauth
          use = "Usage: admin <savestate|ss>\n" ++
                "       admin <deactivate|dv>\n" ++
                "       admin <activate|av>\n" ++
                "       admin <ownerinstall|oi> <true|false>\n" ++
                "       admin <disable|dis> <password>\n" ++
                "       admin <enable|ena> <password>\n" ++
                "       admin <physical|phy> <enable|disable>"

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
cmd_owner :: TPM t => ShellCmd (State t)
cmd_owner = ShellCmd ["owner", "own", "o"]
                    "Perform an ownership command on the TPM"
                    "admin command"
                    (withTPM doowner)
    where doowner s tpm = do
            owner <- parse s
            owner tpm
            return ()
          towner (opass,spass) tpm = do
            let opass' = tpm_digest_pass opass
            let spass' = tpm_digest_pass spass
            (pubkey,hash) <- liftIO $ tpm_key_pubek tpm
            (shn,clo) <- retrieveOIAP tpm
            liftIO $ tpm_takeownership tpm shn pubkey opass' spass'
            closeSession tpm clo shn
            shellPutStrLn $ "Ownership of TPM established successfully"
          cowner opass tpm = do
            let opass' = tpm_digest_pass opass
            (shn,clo) <- retrieveOIAP tpm
            liftIO $ tpm_ownerclear tpm shn opass'
            closeSession tpm clo shn
            shellPutStrLn $ "Ownership of TPM cleared successfully"
          fclear _ tpm = do 
            liftIO $ tpm_forceclear tpm
            shellPutStrLn $ "Ownership of TPM force cleared successfully"
          dforce _ tpm = do
            liftIO $ tpm_disableforceclear tpm
            shellPutStrLn $ "TPM force clearing disabled until reboot"
          downer opass tpm = do
            let opass' = tpm_digest_pass opass
            (shn,clo) <- retrieveOIAP tpm
            liftIO $ tpm_disableownerclear tpm shn opass'
            closeSession tpm clo shn
            shellPutStrLn $ "TPM owner clearing disabled until force clear"
          parse s = liftIO $ witheof s use $ choice [townerp,cownerp,fownclp
                                                    ,dfcownp,docownp]
          townerp = command ["take", "tk"] pass2 towner
          cownerp = command ["clear", "clr"] pass cowner
          fownclp = command ["forceclear", "fc"] none fclear
          docownp = command ["disableownerclear", "doc"] pass downer
          dfcownp = command ["disableforceclear", "dfc"] none dforce
          use = "Usage: owner <take|tk> <owner password> [srk password]\n" ++
                "       owner <clear|clr> <owner password>\n" ++
                "       owner <forceclear|fc>\n" ++
                "       owner <disableownerclear|doc> <owner password>\n" ++
                "       owner <disableforceclear|dfc>"

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
initialize :: (TPM t) => ShellMonad (State t) [ShellCmd (State t)]
initialize = return allcmds

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
allcmds :: (TPM t) => [ShellCmd (State t)]
allcmds = [ cmd_test
          , cmd_admin
          , cmd_owner ]
