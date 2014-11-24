module Network.KVStore.StoreServer (
	serve
) where

import Network.KVStore.DataStore (BinaryStore, insert, get)
import Network.KVStore.Actions (Request(..), Reply(..))
import Network.KVStore.NetPack (netRead, netWrite)
import Network.KVStore.Message (parseRequest, formatResponse)
import Network.KVStore.Exception (handleWith)
import qualified Network as Net
import Control.Monad (forever)
import System.IO (Handle, hClose)
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (atomically)
import Control.Exception (catch, finally, SomeException) 
import System.Timeout (timeout)

serve :: BinaryStore -> IO ()
serve store = Net.withSocketsDo $ do
	sock <- Net.listenOn (Net.PortNumber 9999)
	forever (wait sock store)

wait :: Net.Socket -> BinaryStore -> IO ThreadId
wait sock store = do
	(hdl, _, _) <- Net.accept sock
	let run = (handle hdl store `finally` hClose hdl) `catch` handleWith (return ())
	forkIO $ timeout (10^6) run >> return ()

handle :: Handle -> BinaryStore -> IO ()
handle hdl store = do -- IO
	content <- netRead hdl -- Either String ByteString
	let command = content >>= parseRequest
	print command
	case command of
		Left error -> return () -- Can add error reporting here
		Right (Set k v) -> atomically (insert k v store)
		Right (Get k) -> do
			value <- atomically (get k store)
			let response = case value of
				Just v -> Found k v
				Nothing -> NotFound k
			print response
			netWrite hdl $ formatResponse response
			

