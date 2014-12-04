module Main where

import Network.KVStore.DataStore (BinaryStore, createStore, insert, get)
import Network.KVStore.Actions (Request(Set, Get), Reply(Found, NotFound))
import Network.KVStore.NetPack (netRead, netWrite)
import Network.KVStore.Message (parseRequest, formatResponse)
import Network.KVStore.Exception (handleWith, timeout)
import qualified Network as Net
import System.IO (Handle, hClose)
import Control.Monad (forever)
import Control.Exception (catch, finally) 
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (atomically)

main = do
	globalStore <- atomically createStore
	-- Can load saved store from disk here
	serve globalStore

serve :: BinaryStore -> IO ()
serve store = Net.withSocketsDo $ do
	sock <- Net.listenOn (Net.PortNumber 9999)
	forever (wait sock store)

wait :: Net.Socket -> BinaryStore -> IO ThreadId
wait sock store = do
	(hdl, _, _) <- Net.accept sock
	let run = (handle hdl store `finally` hClose hdl) `catch` handleWith (return ())
	forkIO $ run -- timeout 10 run >> return ()

handle :: Handle -> BinaryStore -> IO ()
handle hdl store = do -- IO
	content <- netRead hdl
	let command = content >>= parseRequest
	case command of
		Left err -> return () -- Can add error reporting here
		Right (Set k v) -> atomically (insert k v store) >> handle hdl store
		Right (Get k) -> do
			value <- atomically (get k store)
			let response = case value of
				Just v -> Found k v
				Nothing -> NotFound k
			netWrite hdl $ formatResponse response
			handle hdl store