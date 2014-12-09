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

import Control.Monad.Trans.Except (ExceptT(..), runExceptT, throwE, except)
import Control.Monad.Trans.Class (lift)
import Control.Monad (when)

netWrite :: Handle -> ByteString -> IO ()
netWrite hdl msg = do
	let len = fromIntegral (BS.length msg) :: Word64
	BS.hPut hdl (encode len)
	BS.hPut hdl msg

netRead :: Handle -> IO (Either String ByteString)
netRead hdl = read --`catch` handleWith (return (Left "Network read error"))
	where read = do
		lengthBytes <- hGet hdl 8
		let length = case decode lengthBytes of
			Right len -> len :: Word64
			Left err -> error "Network stream ended while reading length"
		if length > 10^6 -- Arbitrary limit
			then return $ Left "Message too long"
			else do
				dataBytes <- hGet hdl (fromIntegral length)
				if BS.length dataBytes /= fromIntegral length
					then return $ Left "Connection dropped during message read"
					else return $ Right dataBytes

netRead' :: Handle -> IO (Either String ByteString)
netRead' hdl = runExceptT $ do
		lengthBytes <- lift (hGet hdl 8)
		when (BS.length lengthBytes /= 8) (throwE "Failed to read length")
		length <- ExceptT (return (decode lengthBytes :: Either String Word64))
		let length = fromIntegral length :: Int
		when (length > 10^6) (throwE "Message is too long")
		dataBytes <- lift (hGet hdl length)
		when (BS.length dataBytes /= length) (throwE "Didn't receive entire message")
		return dataBytes