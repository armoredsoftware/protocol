{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}
module CommTools where
import Data.ByteString.Lazy hiding (putStrLn)
import Data.Monoid (mconcat)
import Data.Maybe
import Network.Http.Client
import qualified Network.HTTP.Base as Base
import qualified Network.URI as UR
import Control.Applicative ( (<$>), (<*>), pure )
import qualified Data.HashMap.Strict as HM (member, lookup)
import VChanUtil
import Control.Monad.State.Strict
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
import Control.Concurrent.STM.TMVar
import Control.Monad.STM
import TPM.Types (TPM_PCR_SELECTION, TPM_PCR_COMPOSITE, TPM_IDENTITY_CONTENTS, TPM_PUBKEY)
import qualified ProtoTypesA as Ad 
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
armoredToShared (ANRequestV nreq)           = WNRequest nreq 
armoredToShared (ANResponse nres)           = WNResponse nres
armoredToShared (ArmoredAdam x)             = WAdamData x 
armoredToShared _			    = Result False

armoredToAdam :: Armored -> Ad.ArmoredData
armoredToAdam (ArmoredAdam x) = x 
armoredToAdam x@_             = Ad.AFailure $ "Error: Wrong Armored type converted to ArmoredData: " ++ (show x)

adamToShared :: Ad.ArmoredData -> Shared
adamToShared ad = WAdamData ad 
{-
adamToShared (Ad.ANonce n) = WANonce n
adamToShared (Ad.AEntityInfo ei) = WAEntityInfo ei --EntityInfo
adamToShared (Ad.ACipherText ct)= WACipherText  ct 
adamToShared (Ad.ATPM_PCR_SELECTION t) = WATPM_PCR_SELECTION t
adamToShared (Ad.ATPM_PCR_COMPOSITE t) = WATPM_PCR_COMPOSITE t
adamToShared (Ad.ATPM_IDENTITY_CONTENTS t) = WATPM_IDENTITY_CONTENTS t 
adamToShared (Ad.ATPM_PUBKEY t) = WATPM_PUBKEY t
adamToShared (Ad.ASignedData sd) = WASignedData sd -- (SignedData ArmoredData)
adamToShared (Ad.ASignature s) = WASignature s 
adamToShared (Ad.AEvidenceDescriptor e) = WAEvidenceDescriptor e
adamToShared (Ad.AEvidence e) = WAEvidence e 
-}

sharedToArmored :: Shared -> Armored
sharedToArmored (WRequest req)              = ARequest req
sharedToArmored (WResponse resp)            = AResponse resp
sharedToArmored (WEvidenceDescriptor evdes) = AEvidenceDescriptor evdes
sharedToArmored (WEvidencePiece evpiece)    = AEvidencePiece evpiece
sharedToArmored (WCARequest careq)          = ACARequest careq
sharedToArmored (WCAResponse caresp)	    = ACAResponse caresp
sharedToArmored (WNRequest nreq)            = ANRequestV nreq
sharedToArmored (WNResponse nres)           = ANResponse nres
sharedToArmored (WAdamData ad)              = ArmoredAdam ad 
sharedToArmored x@_			    = AFailure ("attempted to convert to non-supported armored type: " ++ (show x))        

sharedToAdam :: Shared -> Ad.ArmoredData
sharedToAdam (WAdamData d) = d 
sharedToAdam x@_           = Ad.AFailure ("attempted to convert to non-supported ArmoredData type in method sharedToAdam: " ++ (show x))

{-
sharedToAdam (WANonce n) = Ad.ANonce n
sharedToAdam (WAEntityInfo e) = Ad.AEntityInfo e 
sharedToAdam (WACipherText c) = Ad.ACipherText c
sharedToAdam (WATPM_PCR_SELECTION t) = Ad.ATPM_PCR_SELECTION t 
sharedToAdam (WATPM_PCR_COMPOSITE t) = Ad.ATPM_PCR_COMPOSITE t 
sharedToAdam (WATPM_IDENTITY_CONTENTS t) = Ad.ATPM_IDENTITY_CONTENTS t 
sharedToAdam (WATPM_PUBKEY k) = (Ad.ATPM_PUBKEY k)
sharedToAdam (WASignedData sd) = Ad.ASignedData sd
sharedToAdam (WASignature s) = Ad.ASignature s 
sharedToAdam (WAEvidenceDescriptor e) = Ad.AEvidenceDescriptor e 
sharedToAdam (WAEvidence e) = Ad.AEvidence e 
-}


data Shared   = WRequest AD.Request
              | WResponse AD.Response
	      | WEvidenceDescriptor EvidenceDescriptor
	      | WEvidencePiece EvidencePiece
	      | WCARequest CARequest
	      | WCAResponse CAResponse
	      | WNonce Integer 
	      | WCommRequest CommRequest
              | Result Bool
              | VChanFailure String
              | HttpFailure String
              | VChanSuccess String
              | HttpSuccess Port
              | WNRequest NRequest
              | WNResponse NResponse
------------------------------------ADAM DATA
              | WAdamData Ad.ArmoredData
{-
              | WANonce Ad.Nonce
              | WAEntityInfo Ad.EntityInfo
              | WACipherText Ad.CipherText 
              | WATPM_PCR_SELECTION TPM_PCR_SELECTION
              | WATPM_PCR_COMPOSITE TPM_PCR_COMPOSITE
              | WATPM_IDENTITY_CONTENTS TPM_IDENTITY_CONTENTS
              | WATPM_PUBKEY TPM_PUBKEY
              | WASignedData (Ad.SignedData Ad.ArmoredData)
              | WASignature Ad.Signature
              | WAEvidenceDescriptor Ad.EvidenceDescriptor 
              | WAEvidence Ad.Evidence
-}
instance Show Shared where
    show (WRequest app) = "Appraisal: " ++ show app
    show (WResponse att) = "Attestation: " ++ show att
    show (WEvidenceDescriptor evdes) = "EvidenceDescriptor: " ++ (show evdes)
    show (WEvidencePiece evPiece) = "EvidencePiece: " ++ (show evPiece) 
    show (Result True) = "Appraisal succeeded."
    show (Result False) = "Appraisal failed."
    show (VChanFailure str) = "VChanFailure: " ++ str
    show (HttpFailure str)  = "HttpFailure: " ++ str
    show (VChanSuccess str) = "VChanSuccess: " ++ str
    show (HttpSuccess p)  = "HttpSuccess: " ++ (show p)
    show (WNonce n)      = "WNonce: " ++ (show n)
    show (WNRequest nreq) = "WNRequest: " ++ (show nreq)
    show (WNResponse nres) = "WNResponse: " ++ (show nres)

--  show (WCommRequest commreq) = "WCommRequest " ++ (show commreq)
--  show (WPort p) = "WPort: " ++ (show p)
--  show (WPortRequest pr) = "WPortRequest " ++ (show pr)
   
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
	toJSON (VChanFailure str)          = object ["VChanFailure" .= toJSON str]
	toJSON (HttpFailure str)	   = object ["HttpFailure" .= toJSON str]
	toJSON (VChanSuccess str)         =  object ["VChanSuccess" .= toJSON str]
	toJSON (HttpSuccess port)     = object ["HttpSuccess" .= port]
        toJSON (WNRequest nreq)       = object ["WNRequest" .= toJSON nreq]
        toJSON (WNResponse nres)      = object ["WNResponse" .= toJSON nres]
        -------------------------------adam stuff
        toJSON (WAdamData ad)         = object [ "WAdamData" .= toJSON ad]
{-
        toJSON (WANonce n)            = object ["WANonce" .= toJSON n]
       -- toJSON (WAEntityInfo e)       = object ["WAEntityInfo" .= toJSON e]
        toJSON (WACipherText c)       = object ["WACipherText" .= encodeToText (toStrict c)]
        toJSON (WATPM_PCR_SELECTION t)= object ["WATPM_PCR_SELECTION" .= toJSON t]
        toJSON (WATPM_PCR_COMPOSITE t) = object ["WATPM_PCR_SELECTION" .= toJSON t]
        toJSON (WATPM_IDENTITY_CONTENTS i) = object ["WATPM_IDENTITY_CONTENTS" .= toJSON i]
        toJSON (WASignedData s)         = object ["WASignedData" .= toJSON s]
        toJSON (WASignature s)          = object ["WASignature" .=encodeToText (toStrict s)]
        toJSON (WAEvidenceDescriptor e) = object ["WAEvidenceDescriptor" .= toJSON e]
        toJSON (WAEvidence e)           = object ["WAEvidence" .= toJSON e]
-}
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
				| HM.member "VChanFailure" o = VChanFailure <$> o .: "VChanFailure"
				| HM.member "HttpFailure" o = HttpFailure <$> o .: "HttpFailure"
				| HM.member "VChanSuccess" o = VChanSuccess <$> o .: "VChanSuccess"
                                | HM.member "WNRequest" o = WNRequest <$> o .: "WNRequest"
                                | HM.member "WNResponse" o = WNResponse <$> o .: "WNResponse"
------------------------------------------------------adam stuff
                                | HM.member "WAdamData" o = WAdamData <$> o .: "WAdamData"
{-
                                | HM.member "WANonce" o = WANonce <$> o .: "WANonce"
--                                | HM.member "WAEntityInfo"
                                | HM.member "WACipherText" o = WACipherText <$> ((o .: "WACipherText") >>= decodeFromTextL)
                                | HM.member "WATPM_PCR_SELECTION" o = WATPM_PCR_SELECTION <$> o .: "WATPM_PCR_SELECTION"
                                | HM.member "WATPM_PCR_COMPOSITE" o = WATPM_PCR_COMPOSITE <$> o .: "WATPM_PCR_COMPOSITE"
                                | HM.member "WATPM_IDENTITY_CONTENTS" o = WATPM_IDENTITY_CONTENTS <$> o .: "WATPM_IDENTITY_CONTENTS"
                                | HM.member "WASignedData" o = WASignedData <$> o .: "WASignedData"
                                | HM.member "WASignature" o = WASignature   <$> ((o .: "WASignature") >>= decodeFromTextL)
                                | HM.member "WAEvidenceDescriptor" o = WAEvidenceDescriptor <$> o .: "WAEvidenceDescriptor"
                                | HM.member "WAEvidence" o = WAEvidence <$> o .: "WAEvidence"
  -}  
receiveG :: Channel -> IO Armored
receiveG chan = do
 case chan of
  (Channel ent (VChanInfo maybeChan))      -> case maybeChan of
     Nothing -> do
       let str = "ERROR: no vchannel stored!! I can't receive on nothing!"
       putStrLn str
       return (AFailure str)
     Just c  -> do
       eitherShared <- receiveShared c
       case eitherShared of
        Left err -> do
          putStrLn ("ERROR: " ++ err)
          return (AFailure ("RECEIVE MESSAGE FAIL: " ++ err))
        Right shared -> return (sharedToArmored shared)
  (Channel ent (HttpInfo _ _ _ _ maybeConn1 tmvMsgs tmvUnit)) -> do
    putStrLn "Waiting to receive message..."
    unitval <- atomically $ takeTMVar tmvUnit
    msgls <- atomically $ takeTMVar tmvMsgs
    case msgls of
      [] -> do 
        let str = "Error in receive. Was able to take unitTMVar but msglist was empty"
        putStrLn str
        atomically $ putTMVar tmvMsgs msgls
        return (AFailure str)
      (a:[]) -> do 
        --don't put unitTMVar back because list is empty
        --release tmvMsgs
        atomically $ putTMVar tmvMsgs []
        return a
      (a:as) -> do 
        --DO put the unittmvar back this time because there are more messages.
        atomically $ do
                       putTMVar tmvUnit ()
                       putTMVar tmvMsgs as
                       return a
receiveG' :: Channel -> IO Ad.ArmoredData
receiveG' chan = do
 case chan of
  (Channel ent (VChanInfo maybeChan))      -> case maybeChan of
     Nothing -> do
       let str = "ERROR: no vchannel stored!! I can't receive on nothing!"
       putStrLn str
       return (Ad.AFailure str)
     Just c  -> do
       eitherShared <- receiveShared c
       case eitherShared of
        Left err -> do
          putStrLn ("ERROR: " ++ err)
          return (Ad.AFailure ("RECEIVE MESSAGE FAIL: " ++ err))
        Right shared -> return (sharedToAdam shared)
  (Channel ent (HttpInfo _ _ _ _ maybeConn1 tmvMsgs tmvUnit)) -> do
    putStrLn "Waiting to receive message..."
    unitval <- atomically $ takeTMVar tmvUnit
    msgls <- atomically $ takeTMVar tmvMsgs
    case msgls of
      [] -> do 
        let str = "Error in receive. Was able to take unitTMVar but msglist was empty"
        putStrLn str
        atomically $ putTMVar tmvMsgs msgls
        return (Ad.AFailure str)
      (a:[]) -> do 
        --don't put unitTMVar back because list is empty
        --release tmvMsgs
        atomically $ putTMVar tmvMsgs []
        return (armoredToAdam a)
      (a:as) -> do 
        --DO put the unittmvar back this time because there are more messages.
        atomically $ do
                       putTMVar tmvUnit ()
                       putTMVar tmvMsgs as
                       return (armoredToAdam a)
   -- let str = "HTTPINFO??? I don't know what to do with that yet."   
   -- putStrLn str
   -- return (AFailure str)

sendG :: Channel -> Armored -> IO ()
sendG chan armored = do
                     case chan of
                       (Channel ent (VChanInfo maybeChan))      -> case maybeChan of
                         Nothing -> putStrLn "ERROR: no vchannel stored!! I can't send on nothing!"
                         Just c  -> sendShared' c (armoredToShared armored)
                       (Channel ent (HttpInfo _ _ mTheirPort theirIP maybeConn1 _ _)) ->do
                          case mTheirPort of 
                            Nothing -> do
                              let err = "no port of theirs given!!!! I'm trying to send here!!!"
                              putStrLn err
                            Just theirPort -> do 
                              curConn <- sendHttp (armoredToShared armored) theirIP theirPort 
                              putStrLn "Tried to send http!!" -- "HTTPINFO??? I don't know what to do with that yet."

sendG' :: Channel -> Ad.ArmoredData -> IO ()
sendG' chan adam = do
                     case chan of
                       (Channel ent (VChanInfo maybeChan))      -> case maybeChan of
                         Nothing -> putStrLn "ERROR: no vchannel stored!! I can't send on nothing!"
                         Just c  -> sendShared' c (adamToShared adam)
                       (Channel ent (HttpInfo _ _ mTheirPort theirIP maybeConn1 _ _)) ->do
                          case mTheirPort of 
                            Nothing -> do
                              let err = "no port of theirs given!!!! I'm trying to send here!!!"
                              putStrLn err
                            Just theirPort -> do 
                              curConn <- sendHttp (adamToShared adam) theirIP theirPort 
                              putStrLn "Tried to send http!!" -- "HTTPINFO??? I don't know what to do with that yet."

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


logf ::String -> IO ()
logf m = do 
  h <- openFile "log.1" AppendMode
  hPutStrLn h m 
  hClose h

logf' :: String -> ArmoredStateTMonad ()
logf' = (liftIO . logf)

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



