{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ParallelListComp #-}
module Plugins.API (
    module Plugins.API,
    module Text.ParserCombinators.Parsec,
    module TPM
) where
import Shell.API hiding (name)
import Text.ParserCombinators.Parsec hiding (State)
import qualified Text.ParserCombinators.Parsec.Token as Token
import Text.ParserCombinators.Parsec.Language
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Error(ParseError(..))
import Data.Word
import Data.Typeable
import TPM
import Data.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Exception
import Data.List (intersperse)
import Data.ByteString.Lazy (ByteString)
import Data.ByteString.Lazy.Char8 (unpack)

data (TPM t) => State t = State { tpm :: t 
                                , session :: Maybe Session
                                , keys :: [(String,TPM_KEY)]
                                , loaded :: [(String,TPM_KEY_HANDLE)]
                                , sealed :: [(String,TPM_STORED_DATA)]
                                } deriving Typeable

initial :: (TPM tpm) => tpm -> [(String,TPM_KEY)] -> 
                        [(String,TPM_STORED_DATA)] -> State tpm
initial tpm keys sealed = State { 
      tpm = tpm
    , session = Nothing
    , keys = keys
    , loaded = []
    , sealed = sealed }

getTPM = shellGetState >>= return . tpm
getLogging = getTPM >>= return . tpm_logging
setLogging l = shellGetState >>= \st -> shellPutState (st {tpm = lg (tpm st)})
    where lg t = tpm_setlogging t l

getSession = shellGetState >>= return . session
setSession s = shellGetState >>= \st -> shellPutState (st {session = s})
  
getAllKeys = do shellGetState >>= return . keys
setAllKeys k = shellGetState >>= \st -> shellPutState (st {keys = k})

putKey name key = do 
    st <- shellGetState
    shellPutState (st {keys = [(name,key)] ++ (keys st)})
getKey name = do 
    st <- shellGetState
    case lookup name (keys st) of
        Nothing -> liftIO $ throwTPM ("The key " ++ name ++ " does not exist")
        Just k -> return k
    where goodname n = n /= name
removeKey name = do 
    st <- shellGetState
    shellPutState (st {keys = filter goodname (keys st)})
    where goodname (n,_) = n /= name

getAllLoaded = do shellGetState >>= return . loaded
setAllLoaded l = shellGetState >>= \st -> shellPutState (st {loaded = l})

putLoaded name handle = do
    st <- shellGetState
    shellPutState (st {loaded = [(name,handle)] ++ (loaded st)})
getLoaded name = do
    st <- shellGetState
    case lookup name (loaded st) of
        Nothing -> liftIO $ throwTPM ("The key " ++ name ++ " is not loaded")
        Just k  -> return k
removeLoaded name = do 
    st <- shellGetState
    shellPutState (st {loaded = filter goodname (loaded st)})
    where goodname (n,_) = n /= name

getAllSealed = do shellGetState >>= return . sealed
setAllSealed s = shellGetState >>= \st -> shellPutState (st {sealed = s})

putSealed name dat = do
    st <- shellGetState
    shellPutState (st {sealed = [(name,dat)] ++ (sealed st)})
getSealed name = do
    st <- shellGetState
    case lookup name (sealed st) of
        Nothing -> liftIO $ throwTPM ("The key " ++ name ++ " is not sealed")
        Just s  -> return s
removeSealed name = do 
    st <- shellGetState
    shellPutState (st {sealed = filter goodname (sealed st)})
    where goodname (n,_) = n /= name

bs2str :: ByteString -> String
bs2str = unpack

retrieveSession = do
    shn <- getSession
    when (isNothing shn) $ do
        liftIO $ throwTPM "There must be an active session"
    let shn' = fromJust shn
    return shn'

retrieveOIAP tpm = do
    shn <- getSession
    case shn of
        Nothing -> do shn <- liftIO $ tpm_session_oiap tpm
                      setSession (Just shn)
                      return (shn,True)
        Just shn -> case shn of
                        OIAP _ _ -> return (shn,False)
                        _ -> liftIO $ throwTPM msg
    where msg = "The active session must be OIAP"

closeSession tpm cls shn = case cls of
    True -> do liftIO $ tpm_session_close tpm shn
               setSession Nothing
    False -> return ()

withTPM c s = getTPM >>= c s
withOIAP c s = do
    tpm <- getTPM
    (shn,close) <- retrieveOIAP tpm
    close2 <- c s tpm shn
    closeSession tpm (close || close2) shn

langDef = emptyDef { commentStart = ""
                   , commentEnd = ""
                   , commentLine = "#"
                   , nestedComments = False
                   , identStart = letter
                   , identLetter = letter <|> digit <|> char '_'
                   , caseSensitive = True
                   , reservedOpNames = []
                   , reservedNames   = []
                   }

tokenizer = Token.makeTokenParser langDef
identifier = Token.identifier tokenizer
semi = Token.semi tokenizer
parens = Token.parens tokenizer
braces = Token.braces tokenizer
brackets = Token.brackets tokenizer
angles = Token.angles tokenizer
reserved = Token.reserved tokenizer
natural = Token.natural tokenizer
naturalOrFloat = Token.naturalOrFloat tokenizer
integer = Token.integer tokenizer
float = Token.float tokenizer
symbol = Token.symbol tokenizer
reservedOp = Token.reservedOp tokenizer
whiteSpace = Token.whiteSpace tokenizer
hex = Token.hexadecimal tokenizer
lexeme = Token.lexeme tokenizer
semiSep = Token.semiSep tokenizer
semiSep1 = Token.semiSep1 tokenizer
commaSep = Token.commaSep tokenizer
commaSep1 = Token.commaSep1 tokenizer
stringLit = Token.stringLiteral tokenizer

bool_and_pass = bool >>= \b -> pass >>= \p -> return (b,p)
pass_and_bool = pass >>= \p -> bool >>= \b -> return (p,b)
int_and_pass = integer >>= \i -> pass >>= \p -> return (i,p)
name_and_pass = name >>= \id -> mpass >>= \p -> return (id,p)
name_and_pass2 = name >>= \id -> pass2 >>= \(p1,p2) -> return (id,p1,p2)
name_and_data = name_and_pass
name = identifier <?> "name"
pass = stringLit <?> "password"
mpass = option "" pass
pass2 = pass >>= \p1 -> mpass >>= \p2 -> return (p1,p2)
name2_and_pass2 = name2_pass_and_data
name2_pass_and_data = do
    n1 <- name
    n2 <- name
    ps <- pass
    dt <- pass
    return (n1,n2,ps,dt)

enable = (ena <|> dis) <?> "enable or disable"
    where ena = reserved "enable" >> return True
          dis = reserved "disable" >> return False
bool = (tru <|> fls) <?> "boolean"
    where tru = reserved "true" >> return True
          fls = reserved "false" >> return False

none = return ()
command cmds p res = choice (map reserved cmds) >> p >>= return . res

witheof s e p = case res of
                    Left err -> throwTPM (show err ++ "\n\n" ++ e)
                    Right res -> return res
    where res = parse (p >>= \r -> eof >> return r) "" s


readPass p = liftM tpm_digest_pass $ shellGetVerifiedPassLn p
allKeyNames a = (map fst a) ++ ["srk","ek"]
commaKeyNames a = concat $ intersperse ", " a

readKeyName p exists = do
    name <- shellGetLine p
    keys <- getAllKeys
    case (name `elem` allKeyNames keys) /= exists of
        True  -> do shellPutStrLn (msg name keys)
                    readKeyName p exists
        False -> case name of
                    "" -> do shellPutStrLn "Please enter a non-empty key name"
                             readKeyName p exists
                    _  -> return name
    where msg n a = case exists of
                        False -> "The key " ++ n ++ " already exists, " ++
                                 "please choose another name"
                        True  -> "The key " ++ n ++ " does not exist, " ++
                                 "please choose one of: " ++ 
                                 commaKeyNames (map fst a)

readKeyHandle :: TPM tpm => String -> ShellMonad (State tpm) Word32
readKeyHandle p = do
    name <- shellGetLine p
    case name of
        ""      -> do shellPutStrLn "Please enter a non-empty key name"
                      readKeyHandle p
        "srk"   -> return tpm_kh_srk
        "ek"    -> return tpm_kh_ek
        "owner" -> return tpm_kh_owner
        "oper"  -> return tpm_kh_operator
        _       -> do st <- shellGetState
                      case lookup name (loaded st) of
                        Just k  -> return k
                        Nothing -> do shellPutStrLn (err name $ loaded st)
                                      readKeyHandle p
    where err n a = "The key " ++ n ++ " is not loaded, please choose " ++
                    "one of: " ++ (commaKeyNames $ allKeyNames a)

readPCRSelect max p = do
    pcrs  <- shellGetLine p
    pcrs' <- liftIO (witheof pcrs msg $ commaSep integer)
    case validate pcrs' of
        True  -> do let p = (map (\n -> ((fromIntegral n) :: Word8)) pcrs')
                    return $ tpm_pcr_selection max p
        False -> do shellPutStrLn msg
                    readPCRSelect max p
    where msg = "The PCR selection list must be a comma separated list " ++
                "of natural values less than " ++ show max
          validate p = and (map (< max) p)

readPCRComposite tpm p = do
    max <- liftIO $ tpm_getcap_pcrs tpm
    pcrs <- readPCRSelect max p
    comp <- tpm_pcr_composite tpm pcrs
    return comp
