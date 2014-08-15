{-# LANGUAGE DeriveDataTypeable #-}
module TPM.Utils where
import Data.Bits
import Data.Binary
import Data.Typeable
import Control.Exception
import qualified Data.ByteString.Lazy as BS
import Data.List (intersperse)
import TPM.Error
import Data.Word
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------
data TPMException = TPMException String
                  | TPMCode Word32
                    deriving (Typeable)

instance Show TPMException where
    show (TPMException str) = str
    show (TPMCode code) = "TPM ERROR: " ++ mkerr code

instance Exception TPMException

tpmerror str = TPMException str
tpmerrcode code = TPMCode code
throwTPM str = throwIO $ tpmerror str
throwTPMCode code = throwIO $ tpmerrcode code

-------------------------------------------------------------------------------
-- Convert a number into the appropriate hexadecimal character.
-------------------------------------------------------------------------------
tohex :: (Num a, Eq a) => a -> Char
tohex 0  = '0'
tohex 1  = '1'
tohex 2  = '2'
tohex 3  = '3'
tohex 4  = '4'
tohex 5  = '5'
tohex 6  = '6'
tohex 7  = '7'
tohex 8  = '8'
tohex 9  = '9'
tohex 10 = 'A'
tohex 11 = 'B'
tohex 12 = 'C'
tohex 13 = 'D'
tohex 14 = 'E'
tohex 15 = 'F'

-------------------------------------------------------------------------------
-- Convert a bit value into a hexadecimal string.
-------------------------------------------------------------------------------
mkhex :: (Bits a, Num a) => a -> String
mkhex num = reverse $ map (mkhex' num) shifts 
    where shifts = [0,-4 .. -(bitSize num-1)]
          mkhex' num sh = tohex ((shift num (fromIntegral sh)) .&. 0xF)

-------------------------------------------------------------------------------
-- Convert a byte string into a hexadecimal string.
-------------------------------------------------------------------------------
bshex :: BS.ByteString -> String
bshex bs | BS.length bs == 0 = "none"
bshex bs = concat $  intersperse " " (map mkhex (BS.unpack bs))

-------------------------------------------------------------------------------
-- Convert a byte strings into integers
-------------------------------------------------------------------------------
wl2int :: [Word8] -> Integer
wl2int l = foldr unstep 0 (reverse l)
  where unstep b a = a `shiftL` 8 .|. fromIntegral b

bs2int :: BS.ByteString -> Integer
bs2int bs = wl2int (BS.unpack bs)

-------------------------------------------------------------------------------
-- Wrap a long string into evenly sized blocks.
-------------------------------------------------------------------------------
blkwrap _ _ ""  = ""
blkwrap hdr len val = nxt ++ end
    where (nxt,rst) = splitAt len val
          end = if rst == [] then "" else "\n" ++ hdr ++ blkwrap hdr len rst

