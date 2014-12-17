module Main where

import qualified Network as Net
import System.IO (Handle, hClose)
import Data.ByteString (ByteString)
import Control.Monad (forever, unless)
import Control.Concurrent (ThreadId, forkIO)
import Control.Concurrent.STM (STM, atomically)
import Control.Exception (SomeException, catch, finally) 
import Network.KVStore.NetPack (netRead, netWrite)
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
	forkIO (run client)
	where
	run client = (handle client store `finally` hClose client) `catch` exceptionHandler
	exceptionHandler :: SomeException -> IO ()
	exceptionHandler exception = return ()

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
		Nothing -> [NotFound]
		Just v -> [Found v]
processWith store (Delete k) = do
	delete k store
	return []
processWith store (Atomic requests) = do
	results <- mapM (processWith store) requests
	return (concat results)