module Network.KVStore.Exception (
	handleWith,
	timeout
) where

import Control.Exception (SomeException) 
import qualified System.Timeout (timeout)

handleWith :: a -> SomeException -> a
handleWith a _ = a

timeout :: Int -> IO a -> IO (Either String a)
timeout seconds computation = do
	result <- System.Timeout.timeout (seconds*(10^6)) computation
	return $ case result of
		Nothing -> Left "Timeout"
		Just output -> Right output