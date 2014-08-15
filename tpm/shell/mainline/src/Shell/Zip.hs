module Shell.Zip where
import Codec.Archive.Zip
import Data.ByteString.Lazy
import System.Directory
import Prelude hiding (readFile,writeFile)

writeZipFile :: FilePath -> FilePath -> IO ()
writeZipFile path file = do
    oldpath <- getCurrentDirectory
    setCurrentDirectory path
    archive <- addFilesToArchive [OptRecursive] emptyArchive ["."]
    writeFile file (fromArchive archive)
    setCurrentDirectory oldpath

extractZipFile :: FilePath -> FilePath -> IO ()
extractZipFile path file = do
    oldpath <- getCurrentDirectory
    createDirectoryIfMissing True path
    setCurrentDirectory path
    filedata <- readFile file
    extractFilesFromArchive [OptRecursive] (toArchive filedata)
    setCurrentDirectory oldpath
