{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}
module AttesterCAComm where
import Data.ByteString.Lazy
import Data.Monoid (mconcat)
import Data.Maybe
import Network.Http.Client
import qualified Network.HTTP.Base as Base
import qualified Network.URI as UR

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.Aeson as A
import qualified Demo3SharedNOVCHAN as AD

import System.IO
import Data.Word

--foreign export converseWithScottyCA :: AD.CARequest -> IO (Either String AD.CAResponse)

--import qualified System.IO.Streams.Internal as StreamsI
type ID = String


ip="10.100.0.6" -- "192.168.122.1" 
port=3000



converseWithScottyCA :: AD.CARequest -> IO (Either String AD.CAResponse)
converseWithScottyCA req = do
			    Prelude.putStrLn "ENTERING CONVERWITHSCOTTYCA"
			    c <- openConnection ip port
			    Prelude.putStrLn "successfully opened port"
			    q <- buildRequest $ do
			    	  http POST "/"
			    	  setAccept "text/html/json"
			    	  setContentType "application/x-www-form-urlencoded"
			    Prelude.putStrLn ( "Request: " ++ (show req))
			    let nvs = [("request", (toStrict (AD.jsonEncode req)))]
			    Prelude.putStrLn "about to send request"
			    let x = encodedFormBody nvs
			    print "Made it here yaaaaaaaaaaaay"
			    sendRequest c q (x)
			    Prelude.putStrLn "MADE IT HERE2"
			    receiveResponse c (\p i -> do
			    			  x <- Streams.read i
			    			  case x of
			    			     (Nothing) -> return (Left "Error performing Streams.read")
			    			     (Just something) -> do
			    			     			 print something
			    			     			 let caresp = (AD.jsonEitherDecode (fromStrict something) :: Either String AD.CAResponse)
			    			     			 case caresp of
			    			     			 	(Left err) -> return (Left ("Error decoding CAResponse. Error was: " ++ err))
			    			     			 	(Right r)  -> return (Right r)
			    			  )
			    			  --print $ fromMaybe "" x)
			    			  
			    	       

main = do
         Prelude.putStrLn "hello"
         c <- openConnection ip port
         Prelude.putStrLn "Success"
         q <- buildRequest $ do
           http POST "/"
           setAccept "text/html/json"
           setContentType "application/x-www-form-urlencoded"
         Prelude.putStrLn "Success again"
        -- sendRequest c q (fileBody "/home/armored/ca2/myinfo.txt")
         let nvs = [("request",(toStrict (AD.jsonEncode [AD.DONE])))
                   ]
         sendRequest c q (encodedFormBody nvs)
         receiveResponse c (\p i -> do
                         x <- Streams.read i
                         print x
                         print $ fromMaybe "" x
                             )

         closeConnection c

mylift :: a -> IO a
mylift x = return x

--uriAuth = UR.URIAuth "" "129.237.123.78" ":3000" 
--uri = UR.URI "http:" (Just uriAuth) "" "" ""

--f = Base.Request uri Base.POST [] "This is the body."
--rq = Base.mkRequest Base.POST f


doExport' :: String -> AD.CARequest ->  IO ()
doExport' fileName comp =
                   do handle <- openFile fileName WriteMode
                      hPutStrLn handle $ show comp
                      hClose handle
       
       
caFile = "caFile.txt"    
           
readComp' :: IO AD.CARequest
readComp' = do
  handle <- openFile caFile ReadMode
  compString <- hGetLine handle
  let comp :: AD.CARequest
      comp = read compString
  hClose handle
  return comp



