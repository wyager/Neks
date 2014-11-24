module Network.KVStore.NetPack (
	netWritePack,
	netWrite,
	netRead
) where

import Control.Applicative ((<$>))
import Data.Binary.Get (getWord64be, runGet)
import Data.Binary.Put (putWord64be, runPut, putByteString)
import Data.ByteString as BS (ByteString, hGet, length)
import qualified Data.ByteString.Lazy as BSL (ByteString, fromStrict, hPut)
import Control.Exception (catch)
import Network.KVStore.Exception (handleWith)
import System.IO (Handle)

netWritePack :: ByteString -> BSL.ByteString
netWritePack msg = runPut $ putWord64be (fromIntegral $ BS.length msg) >> putByteString msg

netWrite :: Handle -> ByteString -> IO ()
netWrite hdl = BSL.hPut hdl . netWritePack

netRead :: Handle -> IO (Either String ByteString)
netRead hdl = write `catch` handleWith (return (Left "Network read error"))
	where 
	write = do
		length <- runGet getWord64be . BSL.fromStrict <$> hGet hdl 8
		if length < 10^6
			then Right <$> hGet hdl (fromIntegral length)
			else return $ Left "Too much data sent to server"