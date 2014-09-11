module Shell (
    module Shell.API,
    module Shell.Commands.Exit,
    module Shell.Commands.Echo,
    module Shell.Commands.Help,
    module Shell.Commands.Repeat,
    initialShell,runShell,runShellCmd
) where
import Control.Monad.State
import System.Console.Editline.Readline hiding (readHistory,writeHistory)
import Shell.API
import Shell.Commands.Exit
import Shell.Commands.Help
import Shell.Commands.Echo
import Shell.Commands.Repeat
import Shell.Commands.Reload
import qualified Data.Map as Map
import Control.Exception
import Prelude hiding (catch)
import GHC
import GHC.Paths ( libdir )
import DynFlags ( defaultDynFlags, defaultFatalMessager, defaultFlushOut )
import Data.Typeable
import GhcMonad

initialShell st = Shell { exitShell   = False
                        , prompt      = ">"
                        , cmdPrefix   = ":"
                        , cmdBuiltin  = [ exitCommand ["quit","exit"]
                                        , helpCommand ["help","?"]
                                        , reloadCommand ["plugins-reload","plugins"] ]
                        , cmdList     = []
                        , defaultCmd  = repeatCommand ["repeat"]
                        , defaultEval = "echo"
                        , initCmds    = []
                        , shellName   = "the shell"
                        , silent      = False
                        , pluginDirs  = []
                        , pluginInit  = "initialize"
                        , greetMsg    = Nothing
                        , historyFile = Nothing
                        , lastAction  = Nothing
                        , closeShell  = (return ())
                        , userState   = st
                        }

runShell :: Typeable st => Shell st -> IO ((),Shell st)
runShell sh = runShellMonad startShell sh

runShellCmd :: Typeable st => Shell st -> [String] -> IO ((),Shell st)
runShellCmd sh cmd = runShellMonad (execShellCmd cmd) sh

startShell :: Typeable st => StateT (Shell st) IO ()
startShell = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
  runGhcT (Just libdir) $ do
    greet <- liftM greetMsg get
    hist  <- liftM historyFile get
    readHistory hist
    shellStartup
    shellLoadPlugins
    showGreeting greet
    runInitCmds
    shellClearLast
    shellMonad
    writeHistory hist
    where runInitCmds = liftM initCmds get >>= \c -> mapM_ process c

execShellCmd :: Typeable st => [String] -> StateT (Shell st) IO ()
execShellCmd cmd = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
  runGhcT (Just libdir) $ do
    shellLoadPlugins
    mapM_ process cmd

shellMonad :: ShellMonad st ()
shellMonad = do
    state <- get
    let p = prompt state
    case exitShell state of
        True  -> closeShell state
        False -> liftIO (readline p) >>= \maybe -> case maybe of
                    Nothing  -> return ()
                    Just str -> process str >> shellMonad

showGreeting greet = do
    case greet of
        Just g  -> shellPutStrLn g
        Nothing -> return ()
