module Network.KVStore.NetPack (
	netWrite,
	netRead
) where

import Data.ByteString as BS (ByteString, hGet, length, hPut)
import Network.KVStore.Exception (handleWith)
import Data.Serialize (encode, decode)
import Control.Exception (catch)
import System.IO (Handle)
import Data.Word (Word64)

netWrite :: Handle -> ByteString -> IO ()
netWrite hdl msg = do
	let len = fromIntegral (BS.length msg) :: Word64
	BS.hPut hdl (encode len)
	BS.hPut hdl msg

netRead :: Handle -> IO (Either String ByteString)
netRead hdl = do
	lengthBytes <- hGet hdl 8
	case decode lengthBytes of
		Left err -> return (Left "Network stream ended while reading length")
		Right len 	| len < (10^6 :: Word64) -> readData (fromIntegral len)
					| otherwise -> return (Left "Message too long")
	where readData length = do
		dataBytes <- hGet hdl length
		if BS.length dataBytes /= length
			then return (Left "Connection dropped during message read")
			else return (Right dataBytes)