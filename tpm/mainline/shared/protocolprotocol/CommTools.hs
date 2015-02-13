{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}
module CommTools where
import Data.ByteString.Lazy
import Data.Monoid (mconcat)
import Data.Maybe
import Network.Http.Client
import qualified Network.HTTP.Base as Base
import qualified Network.URI as UR
import Control.Applicative ( (<$>), (<*>), pure )
import qualified Data.HashMap.Strict as HM (member, lookup)
import VChanUtil

import System.IO.Streams (InputStream, OutputStream, stdout)
import qualified System.IO.Streams as Streams
import qualified Data.Aeson as A
import Data.Aeson
import qualified Demo3Shared as AD
import Demo3Shared
import ProtoTypes hiding (Result)
import qualified ProtoTypes as ProtoTypes
import System.IO
import Data.Word

--foreign export converseWithScottyCA :: AD.CARequest -> IO (Either String AD.CAResponse)

--import qualified System.IO.Streams.Internal as StreamsI
type ID = String


ip="10.100.0.6" -- "192.168.122.1" 
port=3000

armoredToShared :: Armored -> Shared
armoredToShared (ARequest req)              = WRequest req
armoredToShared (AResponse resp)            = WResponse resp
armoredToShared (AEvidenceDescriptor evdes) = WEvidenceDescriptor evdes
armoredToShared (AEvidencePiece evpiece)    = WEvidencePiece evpiece
armoredToShared (ACARequest careq)          = WCARequest careq
armoredToShared (ACAResponse caresp)	    = WCAResponse caresp
armoredToShared _			    = Result False

sharedToArmored :: Shared -> Armored
sharedToArmored (WRequest req)              = ARequest req
sharedToArmored (WResponse resp)            = AResponse resp
sharedToArmored (WEvidenceDescriptor evdes) = AEvidenceDescriptor evdes
sharedToArmored (WEvidencePiece evpiece)    = AEvidencePiece evpiece
sharedToArmored (WCARequest careq)          = ACARequest careq
sharedToArmored (WCAResponse caresp)	    = ACAResponse caresp
sharedToArmored _			    = AFailure "attempted to convert to non-supported armored type"        


data Shared   = WRequest AD.Request
              | WResponse AD.Response
	      | WEvidenceDescriptor EvidenceDescriptor
	      | WEvidencePiece EvidencePiece
	      | WCARequest CARequest
	      | WCAResponse CAResponse
	      | WNonce Integer
--	      | WPort HttpClient.Port
	      | WCommRequest CommRequest
              | Result Bool
              | VChanFailure
              | HttpFailure
              | VChanSuccess
              | HttpSuccess Port

instance Show Shared where
    show (WRequest app) = "Appraisal: " ++ show app
    show (WResponse att) = "Attestation: " ++ show att
    show (WEvidenceDescriptor evdes) = "EvidenceDescriptor: " ++ (show evdes)
    show (WEvidencePiece evPiece) = "EvidencePiece: " ++ (show evPiece) 
    show (Result True) = "Appraisal succeeded."
    show (Result False) = "Appraisal failed."
    show (VChanFailure) = "VChanFailure"
    show (HttpFailure)  = "HttpFailure"
    show (VChanSuccess) = "VChanSuccess"
    show (HttpSuccess p)  = "HttpSuccess" ++ (show p)
--  show (WCommRequest commreq) = "WCommRequest " ++ (show commreq)
--  show (WPort p) = "WPort: " ++ (show p)
--  show (WPortRequest pr) = "WPortRequest " ++ (show pr)
{-    
instance Binary Shared where
  put (WRequest app)             = do  put (0::Word8)
                                       put app
  put (WResponse att)           = do   put (1::Word8)
                                       put att
  put (Result res)                = do put(2::Word8)
                                       put res

  get = do t<- get :: Get Word8
           case t of
             0 -> do app <- get
                     return (WRequest app)
             1 -> do att <- get
                     return (WResponse att)
             2 -> do res <- get
                     return (Result res)
  -}    
instance ToJSON Shared where
	toJSON (WRequest req) = object [ "WRequest" .= toJSON req]
	toJSON (WResponse resp) = object [ "WResponse" .= toJSON resp ]
	toJSON (Result bool) = object [ "Result" .= toJSON bool]
	toJSON (WEvidenceDescriptor evdes) = object [ "WEvidenceDescriptor" .= toJSON evdes ]
	toJSON (WEvidencePiece evPiece) = object ["WEvidencePiece" .= toJSON evPiece]
	toJSON (WCARequest caRequest) = object [ "WCARequest" .= toJSON caRequest ]
	toJSON (WCAResponse caResponse) = object [ "WCAResponse" .= toJSON caResponse]
	toJSON (WNonce nonce)		= object [ "WNonce" .= nonce]
	toJSON (WCommRequest commreq) = object ["WCommRequest" .= toJSON commreq]
	toJSON (VChanFailure)          = A.String "VChanFailure"
	toJSON (HttpFailure)	      = A.String "HttpFailure"
	toJSON (VChanSuccess)         = A.String "VChanSuccess"
	toJSON (HttpSuccess port)     = object ["HttpSuccess" .= port]

instance FromJSON Shared where
	parseJSON (A.Object o)  | HM.member "WRequest" o = WRequest <$> o .: "WRequest"
				| HM.member "WResponse" o = WResponse <$> o .: "WResponse"
				| HM.member "Result" o = Result <$> o .: "Result"
				| HM.member "WEvidenceDescriptor" o = WEvidenceDescriptor <$> o .: "WEvidenceDescriptor"
				| HM.member "WEvidencePiece" o      = WEvidencePiece <$> o .: "WEvidencePiece"
				| HM.member "WCARequest" o          = WCARequest <$> o .: "WCARequest"
				| HM.member "WCAResponse" o         = WCAResponse <$> o .: "WCAResponse"
				| HM.member "WNonce" o 	            = WNonce <$> o .: "WNonce"
				| HM.member "WCommRequest" o = WCommRequest <$> o .: "WCommRequest"
				| HM.member "HttpSuccess" o  = HttpSuccess <$> o .: "HttpSuccess"
	parseJSON (A.String "VChanFailure") = pure VChanFailure
	parseJSON (A.String "HttpFailure")  = pure HttpFailure
	parseJSON (A.String "VChanSuccess") = pure VChanSuccess	
    
    



sendShared :: Int -> Shared -> IO LibXenVChan
sendShared id shared = do
			chan <- client_init id
			sendShared' chan shared
			return chan

sendShared' :: LibXenVChan -> Shared -> IO ()
sendShared' chan shared = do
			   logger <- createLogger
			   sendChunkedMessageByteString logger chan (toStrict (jsonEncode shared))
			   return ()
			   
receiveShared :: LibXenVChan -> IO (Either String Shared)
receiveShared chan = do
			ctrlWait chan
			logger <- createLogger
			bytes <- readChunkedMessageByteString logger chan
			let shared =  jsonEitherDecode (fromStrict bytes) :: Either String Shared
			return shared			   
sendHttp :: Shared -> Hostname -> Port ->IO Connection 
sendHttp shared iip pport = do
			    c <- openConnection iip pport
			    sendHttp' shared c
			    return c
			    
		    
sendHttp' :: Shared -> Connection -> IO ()
sendHttp' shared c = do
			    q <- buildRequest $ do
			    	  http POST "/"
			    	  setAccept "text/html/json"
			    	  setContentType "application/x-www-form-urlencoded"
			    --Prelude.putStrLn ( "Request: " ++ (show req))
			    let nvs = [("request", (toStrict (AD.jsonEncode shared)))]
			    --Prelude.putStrLn "about to send request"
			    let x = encodedFormBody nvs
			    --print "Made it here yaaaaaaaaaaaay"
			    sendRequest c q (x)
			    return ()
               
receiveHttp :: Connection -> IO (Either String Shared)
receiveHttp c = receiveResponse c (\p i -> do
    			  x <- Streams.read i
    			  case x of
    			     (Nothing) -> return (Left "Error performing Streams.read")
    			     (Just something) -> do
	     			 --print something
	     			 let caresp = (AD.jsonEitherDecode (fromStrict something) :: Either String Shared)
	     			 case caresp of
	     			 	(Left err) -> return (Left ("Error decoding CAResponse. Error was: " ++ err))
	     			 	(Right r)  -> return (Right r)
		  )			    


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



