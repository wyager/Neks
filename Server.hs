module Main where

import Network.KVStore.DataStore (BinaryStore, createStore, insert, get)
import Network.KVStore.Actions (Request(Set, Get), Reply(Found, NotFound))
import Network.KVStore.NetPack (netRead, netWrite)
import Network.KVStore.Message (parseRequests, formatResponses)
import Network.KVStore.Exception (handleWith, timeout)
import qualified Network as Net
import System.IO (Handle, hClose)
import Data.Maybe (catMaybes)
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
	let run = (handle hdl store `finally` hClose hdl) -- `catch` handleWith (return ())
	forkIO run -- $ timeout 10 run >> return ()

handle :: Handle -> BinaryStore -> IO ()
handle hdl store = do
	content <- netRead hdl
	let commands = content >>= parseRequests
	case commands of
		Left err -> print err
		Right commands -> do
			results <- run commands store
			if null results -- Do we need to report anything to the client?
				then return () -- Nothing to send back
				else netWrite hdl $ formatResponses $ results
	handle hdl store

run :: [Request] -> BinaryStore -> IO [Reply]
run []                   _     = return []
run (Set k v : requests) store = do
	atomically (insert k v store)
	run requests store
run (Get k   : requests) store = do
	value <- atomically (get k store)
	let result = case value of
		Just v -> Found k v
		Nothing -> NotFound k
	results <- run requests store
	return (result : results)
