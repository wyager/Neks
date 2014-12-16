module Network.KVStore.Actions (
	Request(Set, Get, Delete, Atomic),
	Reply(Found, NotFound)
) where

import Data.ByteString (ByteString)

data Request = Set ByteString ByteString | Get ByteString | Delete ByteString | Atomic [Request] deriving (Show, Eq)
data Reply = Found ByteString ByteString | NotFound ByteString deriving (Show, Eq)