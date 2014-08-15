{-# LANGUAGE NoMonomorphismRestriction#-}
module Main (main) where
import Shell
import Control.Monad
import Control.Monad.Trans
import System.Environment

envplugs = liftIO $ (getEnv "DLANG_PLUGINS") `catch` \_ -> return "."

breakColon [] = ["."]
breakColon s  = case break (== ':') s of
                    (a,[])    -> [a]
                    (a,':':b) -> a:(breakColon b)

{-
buildPlugin cmd = 1

loadPlugin cmd = do
    paths  <- liftM breakColon envplugs
    liftIO $ putStrLn ("Using Paths: " ++ (show paths))
    liftIO $ putStrLn ("Using Include: " ++ (show (map ("-i"++) paths)))
    status <- makeAll cmd (map ("-i"++) paths)
    case status of
        MakeFailure err   -> mapM_ putStrLn err >> return []
        MakeSuccess _ obj -> do
            plugin <- load_ obj paths "command"
            case plugin of
                LoadFailure err -> mapM_ putStrLn err >> return []
                LoadSuccess _ a -> return [a]

files = ["Shell/Eval.hs", "Shell/LLVM.hs"]
-}


shell = do paths <- liftM breakColon envplugs
           return $ (initialShell ()) {pluginDirs = --paths ++
                                                    ["/Users/peckw/.shell",
                                                     "/Users/peckw/.dlang"]}
           --cmds <- liftM concat $ liftIO (mapM loadPlugin [ x ++ y | x <- paths, y <- files])
           --return $ (\x -> x {cmdList = (cmdList x) ++ cmds, pluginDirs = ["/Users/peckw/.shell", "/Users/peckw/.dlang"]}) (initialShell ())

main = do shell' <- shell
          runShell shell'
          return ()

