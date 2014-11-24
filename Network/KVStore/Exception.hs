module Network.KVStore.Exception (
	handleWith
) where

import Control.Exception (SomeException) 

handleWith :: a -> SomeException -> a
handleWith a _ = a