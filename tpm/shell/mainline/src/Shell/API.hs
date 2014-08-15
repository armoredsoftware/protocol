{-# LANGUAGE
  DeriveDataTypeable,
  FlexibleContexts,
  FlexibleInstances,
  TypeSynonymInstances,
  ExistentialQuantification,
  MultiParamTypeClasses,
  FunctionalDependencies,
  UndecidableInstances,
  GeneralizedNewtypeDeriving,
  ScopedTypeVariables,
  ForeignFunctionInterface,
  StandaloneDeriving
 #-}


module Shell.API where

import System.Console.Editline.Readline hiding (readHistory,writeHistory)
import qualified System.Console.Editline.Readline as Readline
import System.Directory
import System.FilePath
import qualified Data.Map as Map
import Control.Exception

import Control.Monad.State
import Control.Monad.Error

import Prelude hiding (catch)
import Data.List (isPrefixOf,isInfixOf,intersperse,nub)
import Foreign.C
import System.IO
import System.IO.Error hiding (catch)
import System.Time
import Shell.Zip
import Exception(gcatch,IOException,catch,ExceptionMonad,SomeException)

-- The GHC-API
import HscTypes hiding (liftIO)
import Module
import GHC
import Digraph
import qualified MonadUtils
import Outputable
import GHC.Exts
import Data.Typeable
import Data.Dynamic
import GhcMonad

------------------------------------------------------------------------------
-- Definition of shell commands
-- This represents the interface between the pluggable shell and the
-- shell plugins. A shell command is simply a command name, a help
-- string, a detailed description, and a function to execute when the
-- command is used.
------------------------------------------------------------------------------
data ShellCmd st = ShellCmd { name :: [String]
                            , help :: String
                            , desc :: String
                            , cmd  :: String -> ShellMonad st ()
                            } deriving (Typeable)

instance Eq (ShellCmd st) where
    x == y = or $ map (`elem` (name y)) (name x)

------------------------------------------------------------------------------
-- Definition of completion
-- These types are used for the definition of completion functions.
-- These functions can help complete a word for the user when they enter
-- the "complete" key sequence (typically TAB-TAB)
------------------------------------------------------------------------------
type Completion = Maybe (String,[String])
type Completer  = String -> Int -> Int -> IO Completion

------------------------------------------------------------------------------
-- Definition of the shell state
-- This is the definition of the shell state. The shell state is used,
-- and possibly modified, whenever the user type input into the shell.
------------------------------------------------------------------------------
data Shell st = Shell { exitShell   :: Bool
                      , prompt      :: String
                      , cmdPrefix   :: String
                      , silent      :: Bool
                      , cmdBuiltin  :: [ShellCmd st]
                      , cmdList     :: [ShellCmd st]
                      , defaultCmd  :: ShellCmd st
                      , defaultEval :: String
                      , initCmds    :: [String]
                      , shellName   :: String
                      , pluginDirs  :: [String]
                      , pluginInit  :: String
                      , greetMsg    :: Maybe String
                      , historyFile :: Maybe String
                      , lastAction  :: Maybe String
                      , closeShell  :: ShellMonad st ()
                      , userState   :: st
                      } deriving Typeable

------------------------------------------------------------------------------
-- Definition of the shell monad
-- Every shell, and thus every shell command, runs inside of the shell
-- monad. This monad is a simple state monad which maintains the state
-- of the shell.
------------------------------------------------------------------------------
type ShellMonad st a = GhcT (StateT (Shell st) IO) a

instance MonadState st m => MonadState st (GhcT m) where
  get = liftGhcT get
  put  = liftGhcT . put

{-
instance MonadIO m => MonadIO (GhcT m) where
  liftIO = liftGhcT . liftIO
-}

{-
instance MonadUtils.MonadIO (StateT (Shell st) IO) where
  liftIO m = StateT $ \st -> m >>= \v -> return (v,st)
-}

instance ExceptionMonad m => ExceptionMonad (StateT st m) where
  gcatch m h = StateT $ \st -> runStateT m st `gcatch` (\e -> runStateT (h e) st)

{-
instance (Typeable1 m, Typeable a) => Typeable (GhcT m a) where
  typeOf _ = mkTyConApp (mkTyCon "HscTypes.GhcT") [typeOf1 (undefined :: m a), typeOf  (undefined :: a)]

instance (Typeable st, Typeable1 m) => Typeable1 (StateT st m) where
  typeOf1 _ = mkTyConApp (mkTyCon "Control.Monad.State.Lazy.StateT")
                         [typeOf (undefined :: st), typeOf1 (undefined :: (m ()))]
-}
deriving instance Typeable GhcT
deriving instance Typeable StateT


------------------------------------------------------------------------------
------------------------------------------------------------------------------
runShellMonad = runStateT
runSh st m = do stsave <- get
                put st
                v <- m
                st' <- get
                put stsave
                return (st',v)

------------------------------------------------------------------------------
-- Definition of shell API functions
------------------------------------------------------------------------------
primaryName cmd     = (name cmd) !! 0

silentPutStr a      = do
    sil <- liftM silent get
    case sil of
        True  -> return ()
        False -> liftIO $ putStr a

silentPutStrLn a      = do
    sil <- liftM silent get
    case sil of
        True  -> return ()
        False -> liftIO $ putStrLn a

shellPutStr a     = liftIO $ putStr a
shellPutStrLn a   = liftIO $ putStrLn a
shellGetLine p    = liftIO $ (putStr p >> hFlush stdout >> getLine)
shellGetLineLn p  = shellGetLine p >>= \l -> (liftIO (putStrLn "")) >> return l
shellReadLine p   = liftIO $ readline p
shellReadLineLn p = liftIO $ (readline p >>= \r -> putStrLn "" >> return r)
shellGetPass p    = withEchoOff $ shellGetLine p
shellGetPassLn p  = withEchoOff $ shellGetLineLn p
shellGetVerifiedPassLn p = withEchoOff $ do
    p1 <- shellGetLineLn p
    p2 <- shellGetLineLn ("Verify " ++ p)
    case p1 == p2 of
        True  -> return p1
        False -> do liftIO $ (putStrLn "Passwords did not match.\n")
                    shellGetVerifiedPassLn p

shellAddHistory a = liftIO $ addHistory a

shellGetCommands :: ShellMonad st [ShellCmd st]
shellGetCommands = liftM cmdList get

shellSetCommands :: [ShellCmd st] -> ShellMonad st ()
shellSetCommands c = modify (\x -> x {cmdList = c})

shellWithCommands :: ([ShellCmd st] -> [ShellCmd st]) -> ShellMonad st ()
shellWithCommands f = shellGetCommands >>= return . f >>= shellSetCommands

shellAddCommand :: ShellCmd st -> ShellMonad st ()
shellAddCommand c = do
    cmds <- shellGetCommands
    case c `elem` cmds of
        True  -> shellPutStrLn $ "The command '" ++ (primaryName c) ++
                                 "' is already provided"
        False -> shellWithCommands (++[c])

shellPluginDirs :: ShellMonad st [String]
shellPluginDirs = liftM pluginDirs get

shellPluginInit :: ShellMonad st String
shellPluginInit = liftM pluginInit get

shellExit :: ShellMonad st ()
shellExit = modify (\x -> x {exitShell = True})

shellSetLast :: String -> ShellMonad st ()
shellSetLast a = modify (\x -> x {lastAction = Just a})

shellClearLast :: ShellMonad st ()
shellClearLast = modify (\x -> x {lastAction = Nothing})

shellPrompt :: String -> ShellMonad st ()
shellPrompt p = modify (\x -> x {prompt = p})

shellGetState :: ShellMonad st st
shellGetState = liftM userState get

shellPutState :: st -> ShellMonad st ()
shellPutState st = modify (\x -> x {userState = st})

shellWithState :: (st -> st) -> ShellMonad st ()
shellWithState f = shellGetState >>= return . f >>= shellPutState

shellRepeat :: ShellMonad st ()
shellRepeat = do
    action <- liftM lastAction get
    case action of
        Nothing -> return ()
        Just a  -> process a

shellClearPlugins :: ShellMonad st ()
shellClearPlugins = do
    shellPutStrLn $ "Clearing all Plugins"
    builtin <- liftM cmdBuiltin get
    shellSetCommands builtin
    workingDirectoryChanged
    abandonAll
    setTargets []
    ok <- load LoadAllTargets
    workingDirectoryChanged
    return ()

shellLoadPlugins :: Typeable st => ShellMonad st ()
shellLoadPlugins = do
    st <- get
    shellClearPlugins
    shellLoadPlugins' `catchIO` error st
    where error st err = do
            shellPutStrLn (show err)

shellStartup :: ShellMonad st ()
shellStartup = do
    flags <- getSessionDynFlags
    setSessionDynFlags (flags { ghcLink = LinkInMemory })
    return ()

shellLoadPlugins' :: Typeable st => ShellMonad st ()
shellLoadPlugins' = do
    modGraph' <- getModuleGraph
    shellPutStrLn "Running Shell Loading Code"
    plgdirs <- shellPluginDirs
    stagebuild plgdirs
    -- findbuildable returns a list of (path,file) targets. The path needs
    -- to be added to the dynamic import path flags, and the file needs to
    -- be added as a target
    bldplgs <- liftIO $ findbuildable plgdirs
    dflags <- getSessionDynFlags
    setSessionDynFlags (mod dflags bldplgs)
    -- Use GHC to figure out what the actual targets are.
    targets <- mapM (\x -> guessTarget x Nothing) (map snd bldplgs)
    setTargets targets
    -- Load all of the targets, compiling them if necessary
    depanal [] True
    load LoadAllTargets
    -- After loading
    modGraph <- getModuleGraph
    let graph' = flattenSCCs (topSortModuleGraph True modGraph Nothing)
    setContext (map (IIModule . moduleName . ms_mod) graph')
    -- Execute the plugin initialization
    shellPutStrLn "About to call inits"
    setSessionDynFlags dflags
    mapM loadplugin $ filter (isTarget targets) (map ms_mod graph')
    cleanstage plgdirs
    setCompleter
    return ()
    where mod d b = d { importPaths = (importPaths d) ++ nub (map fst b) }

shownam = show . moduleNameString . ms_mod_name

isTarget :: [Target] -> Module -> Bool
isTarget targets mod =  moduleNameString (moduleName mod) `elem` targetNames
  where targetName (TargetModule mn) = moduleNameString mn
        targetName (TargetFile fp _) = reverse $ takeWhile (/= '/') $ tail $ dropWhile (/= '.') $ reverse fp
        targetNames = map (  targetName . targetId )  targets


showHelp :: ShellMonad st ()
showHelp = do
    help <- liftM (map help . cmdList) get
    name <- liftM (map primaryName . cmdList) get
    prfx <- liftM cmdPrefix get
    let name' = map (pad (max name)) name
    mapM (shellPutStrLn . (uncurry $ format prfx)) (zip name' help)
    return ()
    where format p n h = p ++ n ++ "    " ++ h
          check x y = if x > (length y) then x else (length y)
          max name  = foldl check 0 name
          pad max n = let ln = length n in if ln >= max then n else n ++ (pad' max ln)
          pad' m n  = take (m - n) (repeat ' ')

showDesc :: String -> ShellMonad st ()
showDesc cmd = do
    cmds <- liftM cmdList get
    let matches = findcmds cmds cmd
    case matches of
        [] -> shellPutStrLn $ "No help found for: '" ++ cmd ++ "'"
        _  -> mapM (shellPutStrLn . format) matches >> return ()
    where format c = "Command: " ++ (concat $ intersperse ", " (name c)) ++
                     "\n" ++ break ++ (desc c)
          break    = (take 70 (repeat '-')) ++ "\n"

------------------------------------------------------------------------------
-- Definition of internal functions
-- The following functions are used internally by the shell API. They
-- should not be exposed as API functions which can be called by shell
-- commands, however. Most of these functions are wrapped by a nicer to
-- use API interface anyways.
------------------------------------------------------------------------------
setCompleter :: ShellMonad st ()
setCompleter = do
    complt <- cmdCompleter
    liftIO $ setAttemptedCompletionFunction (Just complt)

cmdCompleter :: ShellMonad st (Completer)
cmdCompleter = do
        pref <- liftM cmdPrefix get
        cmds <- shellGetCommands
        return $ comp (complt pref cmds)
    where complt pref cmds = map ((pref++) . primaryName) cmds
          comp complt [] _ _ = return $ Nothing
          comp complt word begin end = do
            buffer <- getLineBuffer
            case found of
                []  -> return $ Nothing
                [x] -> return $ Just (x,[x])
                xs  -> return $ Just (word,xs)
            where before buf = take begin buf
                  after  buf = drop end buf
                  found      = filter (isPrefixOf word) complt

findcmds cmds []  = []
findcmds cmds str = [c | c <- cmds, find' (name c)]
    where find' cmd = or $ map (\x -> (take (length str') x) == str') cmd
          str'      = (words str) !! 0

process :: String -> ShellMonad st ()
process str = do
    st <- get
    (process' str) `catchIO` error st
    where error st err = do
            shellPutStrLn (show err)

process' :: String -> ShellMonad st ()
process' [] = return ()
process' str = do
    shellAddHistory str
    prefix <- liftM cmdPrefix get
    defaul <- liftM defaultEval get
    run prefix defaul
    where run prefix defaul =
            if (take (length prefix) str) == prefix
                then processcmd (drop (length prefix) str)
                else shellSetLast str >> processcmd (defaul ++ " " ++ str)
          report err = shellPutStrLn (ioeGetErrorString err)

processcmd []  = do
    defaul <- liftM (cmd . defaultCmd) get
    defaul []

processcmd str = do
    p    <- liftM cmdPrefix get
    cmds <- liftM cmdList get
    let matches = findcmds cmds str
    case matches of
        []    -> shellPutStrLn $ "The command '" ++ str ++ "' is not valid"
        (a:_) -> shellSetLast (p++str) >> (cmd a) rem
    where rem = strip (drop (length ((words str) !! 0)) str)
          strip x    = dropWhile whtsp x
          whtsp ' '  = True
          whtsp '\t' = True
          whtsp _    = False

findbuildable paths = do
    liftM concat $ mapM allnames (spaths++(zip paths paths))
    where allnames (l,x) = catchIO (allnames' x >>= zipup l) (\_ -> return [])
          allnames' x = liftM (map (x </>)) (getDirectoryContents x)
          isbuild x   = doesFileExist x >>= return . (&& ishs x)
          ishs x      = (take 3 (reverse x)) == "sh."
          mkstage p   = (p, p </> ".stage")
          spaths      = map mkstage paths
          zipup x ys  = filterM isbuild ys >>= return . zip (repeat x)

findloadable paths = do
    contents <- liftM concat $ mapM allnames (cpaths++paths)
    filterM isload contents
    where allnames x  = catchIO (allnames' x) (\_ -> return [])
          allnames' x = liftM (map (x </>)) (getDirectoryContents x)
          isload x    = doesFileExist x >>= return . (&& isobj x)
          isobj x     = (take 2 (reverse x)) == "o."
          mkcache p   = p </> ".cache"
          cpaths      = map mkcache paths

cleanstage :: [FilePath] -> ShellMonad st ()
cleanstage paths = liftIO $ mapM_ remove spaths
    where mkstage p   = p </> ".stage"
          spaths      = map mkstage paths
          remove p    = catchIO (removeDirectoryRecursive p) (\_ -> return ())

stagebuild :: [FilePath] -> ShellMonad st ()
stagebuild paths = do
    zips <- liftIO $ filezips paths
    liftIO $ mapM_ extract zips
    stagebinary paths
    where filezips path = do
            contents <- liftM concat $ mapM allnames paths
            filterM check contents
          allnames p = catchIO (allnames' p) (\_ -> return [])
          allnames' p = liftM (map (p </>)) (getDirectoryContents p)
          check p = doesFileExist p >>= return . (&& iszip p)
          iszip p = (take 4 (reverse p)) == "piz."
          mktouch p = (takeDirectory p) </> ".cache" </> (takeFileName p)
          extract p = isnewer p (mktouch p) >>= \x -> case x of
                        True  -> extract' p
                        False -> return ()
          extract' p = do extractZipFile ((takeDirectory p) </> ".stage") p
                          touchfile (mktouch p)

safeDirectoryContents p = catchIO (getDirectoryContents p) (\_ -> return [])

safeDirContents path add = do
    con   <- catchIO (getDirectoryContents path) (\_ -> return [])
    files <- filterM (isfile path) con
    dirs  <- filterM (isdir path) con
    return (map (add</>) files,map (add</>) dirs)
    where isfile p f = doesFileExist (p</>f) >>= return . ((&&) (not (ishid f)))
          isdir  p f = doesDirectoryExist (p</>f) >>= return . ((&&) (not (ishid f)))
          ishid  p   = isPrefixOf "." p

copyfile old new = do
    createDirectoryIfMissing False (takeDirectory new)
    copyFile old new

stagebinary :: [FilePath] -> ShellMonad st ()
stagebinary paths = do
    allfiles <- liftIO $ mapM (files "") stage
    let results = concat $ map form (zip paths (zip stage allfiles))
    liftIO $ mapM_ (uncurry copyfile) results
    where files add path = do
            (fils,dirs) <- safeDirContents path add
            subfiles <- mapM (uncurry files) (zip dirs (map (path</>) dirs))
            return $ fils ++ (concat subfiles)
          stage     = map (</> ".stage") paths
          form (p,(s,f)) = zip (map (s</>) f) (map ((p</>".cache")</>) f)

touchfile :: FilePath -> IO ()
touchfile path = do
    createDirectoryIfMissing False (takeDirectory path)
    catchIO (writeFile path "") (\_ -> return ())

isnewer :: FilePath -> FilePath -> IO Bool
isnewer left right = return True

loadplugin :: Typeable st => Module -> ShellMonad st ()
loadplugin mod = do
    plginit <- shellPluginInit
    let mname = moduleNameString (moduleName mod)
    let initName = mname ++ "." ++ plginit
    init <- dynCompileExpr initName
    case fromDynamic init of
      Just initialize -> do
         silentPutStr $ "Loaded Plugin: "
         silentPutStrLn $ mname
         pcs <- initialize
         mapM_ shellAddCommand pcs
         --mapM_ (shellAddMod . (\x->(mod,x))) pcs

      Nothing -> shellPutStrLn "Could not cast initialization function"

readHistory hist = do
    case hist of
        Nothing -> return ()
        Just x  -> do shellPutStrLn $ "Reading history: " ++ (show x)
                      liftIO $ Readline.readHistory x
                      return ()

writeHistory hist = do
    case hist of
        Nothing -> return ()
        Just x  -> do shellPutStrLn $ "Writing history: " ++ (show x)
                      liftIO $ Readline.writeHistory x
                      return ()

catchIO :: ExceptionMonad m => m a -> (SomeException -> m a) -> m a
catchIO = gcatch

withEchoOff c = do old <- liftIO $ hGetEcho stdin
                   liftIO $ hSetEcho stdin False
                   r <- c
                   liftIO $ hSetEcho stdin old
                   return r
