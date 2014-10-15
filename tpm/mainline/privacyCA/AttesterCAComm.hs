{-# LANGUAGE OverloadedStrings #-}
import Data.ByteString.Lazy
import Data.Monoid (mconcat)
import Data.Maybe
import Network.Http.Client
import qualified Network.HTTP.Base as Base
import qualified Network.URI as UR

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.Aeson as A
import CADataTypes
--import qualified System.IO.Streams.Internal as StreamsI
type ID = String
main = do
         Prelude.putStrLn "hello"
         c <- openConnection "129.237.123.78" 3000
         Prelude.putStrLn "Success"
         q <- buildRequest $ do
           http POST "/"
           setAccept "text/html/json"
           setContentType "application/x-www-form-urlencoded"
         Prelude.putStrLn "Success again"
        -- sendRequest c q (fileBody "/home/armored/ca2/myinfo.txt")
         let nvs = [("request",(toStrict (jsonEncode example)))
                   ]
         sendRequest c q (encodedFormBody nvs)
         receiveResponse c (\p i -> do
                         x <- Streams.read i
                         print $ fromMaybe "" x)

         closeConnection c

mylift :: a -> IO a
mylift x = return x

--uriAuth = UR.URIAuth "" "129.237.123.78" ":3000" 
--uri = UR.URI "http:" (Just uriAuth) "" "" ""

--f = Base.Request uri Base.POST [] "This is the body."
--rq = Base.mkRequest Base.POST f

