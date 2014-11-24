module Main where

import Network.KVStore.DataStore (BinaryStore, createStore)
import Network.KVStore.StoreServer (serve)
import Control.Concurrent.STM (atomically)

main = do
	globalStore <- atomically createStore
	-- Can load saved store from disk here
	serve globalStore
	putStrLn "hello"
