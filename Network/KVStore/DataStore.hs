module Network.KVStore.DataStore (
	DataStore,
	createStore,
	insert,
	get
) where

import qualified Data.Map.Strict as Map (Map, empty, insert, lookup)
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TMVar (TMVar, newTMVar, takeTMVar, putTMVar, readTMVar)
import Data.Hashable (Hashable)
import Data.Vector (Vector, fromList, (!))
import Data.Hashable (hash)
import Data.Bits ((.&.))

newtype DataStore k v = DataStore (Vector (TMVar (Map.Map k v)))

createStore :: STM (DataStore k v)
createStore = do
	maps <- sequence [newTMVar Map.empty | _ <- [0..255]]
	return (DataStore (fromList maps))

insert :: (Hashable k, Ord k) => k -> v -> DataStore k v -> STM ()
insert k v (DataStore maps) = do
	let atomicMap = maps ! (hash k .&. 0xFF)
	map <- takeTMVar atomicMap
	putTMVar atomicMap $! (Map.insert k v map)

get :: (Hashable k, Ord k) => k -> DataStore k v -> STM (Maybe v)
get k (DataStore maps) = do
	let atomicMap = maps ! (hash k .&. 0xFF)
	map <- readTMVar atomicMap
	return (Map.lookup k map)