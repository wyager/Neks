module Network.KVStore.NetPack (
	netWrite,
	netRead
) where

import Data.ByteString as BS (ByteString, hGet, length, hPut)
import Network.KVStore.Exception (handleWith)
import Data.Serialize (encode, decode)
import Control.Applicative ((<$>))
import Control.Exception (catch)
import System.IO (Handle)
import Data.Word (Word64)

netWrite :: Handle -> ByteString -> IO ()
netWrite hdl msg = do
	let len = fromIntegral (BS.length msg) :: Word64
	BS.hPut hdl (encode len)
	BS.hPut hdl msg

netRead :: Handle -> IO (Either String ByteString)
netRead hdl = read `catch` handleWith (return (Left "Network read error"))
	where read = do
		lengthBytes <- hGet hdl 8
		let length = case decode lengthBytes of
			Right len -> len :: Word64
			Left err -> error "How the hell would a Word64 fail to decode?"
		if length < 10^6 -- Arbitrary limit
			then Right <$> hGet hdl (fromIntegral length)
			else return $ Left "Too much data sent to server"