module Network.KVStore.Disk (saveTo, loadFrom) where

import Control.Concurrent.STM (atomically)
import Network.KVStore.DataStore (DataStore, dump, load)
import Network.KVStore.NetPack (netRead, netWrite) -- also works on files
import System.IO (Handle, withFile, IOMode(ReadMode, WriteMode))
import System.Directory (renameFile, doesFileExist)
import Data.Serialize (Serialize, encode, decode)
import Control.Applicative ((<$>))

saveTo :: (Serialize a, Ord a, Serialize b) => String -> DataStore a b -> IO ()
saveTo path store = do
	withFile (path ++ "~") WriteMode (`saveToHandle` store)
	renameFile (path ++ "~") path
saveToHandle handle store = do
	maps <- atomically (dump store)
	sequence_ [netWrite handle (encode map) | map <- maps]

loadFrom :: (Serialize a, Ord a, Serialize b) => String -> IO (Maybe (DataStore a b))
loadFrom path = doesFileExist path >>= \exists -> if exists
	then Just <$> withFile path ReadMode loadFromHandle
	else return Nothing
loadFromHandle handle = do
	maps <- sequence . replicate 4096 $ do
		mapData <- netRead handle
		let Right map = mapData >>= decode
		return map
	atomically (load maps)