import Database.HDBC 
import Database.HDBC.Sqlite3
import ScottyCA

import Web.Scotty
import Data.ByteString.Lazy (ByteString, append, empty, pack, length, toStrict, fromStrict)
import qualified Data.ByteString.Lazy.Char8 as L

import Data.Binary
import System.IO
import Crypto.Cipher.AES
import Control.Monad.Trans
import qualified Data.Text.Lazy as LazyText
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy.Encoding as LazyEncoding
import qualified Demo3Shared as AD  --ArmoredData
import TPM



main = do
	putStrLn "hello"
	conn <- connectSqlite3 "armoredDB.db"
	let id = show (19 :: Int)
	res <- quickQuery' conn ("SELECT jsontpmkey from pubkeys where id =" ++ id) []
	putStrLn (show res) 
	putStrLn "success "
	filekey <- readPubEK
	run conn "INSERT INTO pubkeys VALUES( ?, ?)" [toSql id, toSql (AD.jsonEncode filekey)]
	commit conn
	disconnect conn
	putStrLn (show (AD.jsonEncode filekey))
