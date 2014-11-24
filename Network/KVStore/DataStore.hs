module Network.KVStore.DataStore (
	BinaryStore,
	createStore,
	insert,
	get
) where

import qualified Data.Map.Strict as Map (Map, empty, insert, lookup) -- insert, member, elems, keys
import Control.Concurrent.STM (STM) -- atomically
import Control.Concurrent.STM.TMVar (TMVar, newTMVar, takeTMVar, putTMVar, readTMVar)
import Data.ByteString (ByteString)

newtype DataStore k v = DataStore {mapOf :: TMVar (Map.Map k v)}

type BinaryStore = DataStore ByteString ByteString

createStore :: Ord k => STM (DataStore k v)
createStore = do
	map <- newTMVar Map.empty
	return (DataStore map)

insert :: Ord k => k -> v -> DataStore k v -> STM ()
insert k v store = do
	map <- takeTMVar (mapOf store)
	putTMVar (mapOf store) $! (Map.insert k v map)

get :: Ord k => k -> DataStore k v -> STM (Maybe v)
get k store = do
	map <- readTMVar (mapOf store)
	return (Map.lookup k map)