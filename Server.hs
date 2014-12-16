module Main where

import qualified Network as Net
import System.IO (Handle, hClose)
import Data.ByteString (ByteString)
import Control.Monad (forever, unless)
import Control.Exception (catch, finally) 
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (STM, atomically)
import Network.KVStore.NetPack (netRead, netWrite)
import Network.KVStore.Exception (handleWith, timeout)
import Network.KVStore.Message (parseRequests, formatResponses)
import Network.KVStore.DataStore (DataStore, createStore, insert, get, delete)
import Network.KVStore.Actions (Request(Set, Get, Delete, Atomic), Reply(Found, NotFound))

type Store = DataStore ByteString ByteString

main = do
	globalStore <- atomically createStore
	-- Can load saved store from disk here
	serve globalStore

serve :: Store -> IO ()
serve store = Net.withSocketsDo $ do
	sock <- Net.listenOn (Net.PortNumber 9999)
	forever (wait sock store)

wait :: Net.Socket -> Store -> IO ThreadId
wait sock store = do
	(client, _, _) <- Net.accept sock
	let run = (handle client store `finally` hClose client) `catch` handleWith (return ())
	forkIO run -- $ timeout 10 run >> return ()

handle :: Handle -> Store -> IO ()
handle client store = do
	result <- processRequests client store
	case result of
		Right success -> handle client store
		Left failure -> return () -- print failure -- Error reporting

processRequests :: Handle -> Store -> IO (Either String ())
processRequests client store = do
	requestData <- netRead client
	case requestData >>= parseRequests of
		Left err -> return (Left err)
		Right requests -> do
			results <- mapM (atomically . processWith store) requests
			netWrite client . formatResponses . concat $ results
			return (Right ())

processWith :: Store -> Request -> STM [Reply]
processWith store (Set k v) = do
	insert k v store
	return []
processWith store (Get k) = do
	result <- get k store
	return $ case result of
		Nothing -> [NotFound k]
		Just v -> [Found k v]
processWith store (Delete k) = do
	delete k store
	return []
processWith store (Atomic requests) = do
	results <- mapM (processWith store) requests
	return (concat results)