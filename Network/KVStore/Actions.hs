module Network.KVStore.Actions (
	Request(Set, Get),
	Reply(Found, NotFound)
) where

import Data.ByteString (ByteString)

data Request = Set ByteString ByteString | Get ByteString deriving (Show, Eq)
data Reply = Found ByteString ByteString | NotFound ByteString deriving (Show, Eq)