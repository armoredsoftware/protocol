{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module CADataTypes where
   
--import Demo2Shared as D2
import Data.Aeson
import qualified Data.Aeson as DA
import Data.Aeson.TH
import Data.Map
import Data.Text
import Data.Word
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.ByteString as B

type ID = Int
type AIK_pub = [Word8]
type AIK_pub_sig = [Word8]

data CARequest_json = CAReq_json {
			getID          :: ID,
                        getAIK_pub     :: AIK_pub,
                        getAIK_pub_sig :: AIK_pub_sig
                        } deriving (Show, Eq)
data CAResponse_json = CAResp_json {
			getSk_encryptedW_EK :: [Word8],
			getAIK_signedCA_encrytpedW_Sk :: [Word8]
			}
		     | CARespFailure_json{
			getReason :: String
			}


$(deriveJSON defaultOptions ''CARequest_json)
$(deriveJSON defaultOptions ''CAResponse_json)

example= CAReq_json 18 (toWord8s "key") (toWord8s "sig")

toWord8s :: String -> [Word8]
toWord8s str = (LB.unpack (LBC.pack str))


jsonEncode :: (ToJSON a) => a -> LB.ByteString
jsonEncode = DA.encode

jsonDecode :: (FromJSON a) =>  LB.ByteString -> Maybe a
jsonDecode= DA.decode
