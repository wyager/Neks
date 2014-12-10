module Main where

import Network.KVStore.DataStore (BinaryStore, createStore, insert, get)
import Network.KVStore.Actions (Request(Set, Get), Reply(Found, NotFound))
import Network.KVStore.NetPack (netRead, netWrite)
import Network.KVStore.Message (parseRequests, formatResponses)
import Network.KVStore.Exception (handleWith, timeout)
import qualified Network as Net
import System.IO (Handle, hClose)
import Control.Monad (forever, unless)
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
	(client, _, _) <- Net.accept sock
	let run = (handle client store `finally` hClose client) `catch` handleWith (return ())
	forkIO run -- $ timeout 10 run >> return ()

handle :: Handle -> BinaryStore -> IO ()
handle client store = do
	result <- processCommand client store
	case result of
		Right success -> (handle client store)
		Left failure -> return () -- print failure -- Error reporting

processCommand :: Handle -> BinaryStore -> IO (Either String ())
processCommand client store = do
	commandData <- netRead client
	case commandData >>= parseRequests of
		Left err -> return (Left err)
		Right commands -> do
			sequence [atomically (insert k v store) | Set k v <- commands]
			results <- sequence [lookup k | Get k <- commands]
			unless (null results) (netWrite client $ formatResponses $ results)
			return (Right ())
	where lookup k = do
		result <- atomically (get k store)
		return $ case result of
			Nothing -> NotFound k
			Just v -> Found k v