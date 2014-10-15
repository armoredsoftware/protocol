{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text.Lazy as LazyText
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy.Encoding as LazyEncoding
import CADataTypes
import qualified Demo3Shared as Adam
import qualified PrivacyCA as CAHelper

main = scotty 3000 $ do
  --  get "/" $ text "foobar"
    get "/foo" $ do
      v <- param "fooparam"
      html $ mconcat ["<h1>", v, "</h1>"]
      

    post "/" $ do
  --    req <- request
     --   bod <- body
       -- a <- jsonData :: ActionM String
   --     text  a
        --json (bod :: String) --(a :: String)
      a <- (param "request") :: ActionM LazyText.Text
      
      html a
      let jj = (jsonDecode (LazyEncoding.encodeUtf8 a) :: Maybe CARequest_json)
      case jj of
	   Nothing -> text "you suck"
	   Just caReq_json -> json (handleCAReq_json caReq_json)
      --return ()
      --text "posted!"
   --     text (LazyText.pack (L.unpack bod))
   
handleCAReq_json :: CARequest_json -> CAResponse_json
handleCAReq_json