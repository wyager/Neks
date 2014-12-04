module Network.KVStore.Actions (
	Request(..),
	Reply(..)
) where

import Data.ByteString (ByteString)

data Request = Set {k :: ByteString, v :: ByteString} | 
			   Get {k :: ByteString} deriving (Show)

data Reply = Found {foundKey :: ByteString, foundValue :: ByteString} |
			 NotFound {foundKey :: ByteString} deriving (Show)