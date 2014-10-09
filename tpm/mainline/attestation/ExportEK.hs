import System.IO
import TPM

doExport :: String -> TPM_PUBKEY -> IO ()
doExport fileName pubKey = do
  handle <- openFile fileName WriteMode
  hPutStrLn handle $ show pubKey
  hClose handle