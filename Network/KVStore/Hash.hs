module Network.KVStore.Hash (
	hash
) where

import Data.ByteString as BS (ByteString, foldl)
import Data.Bits ((.&.))

-- Produces a number in the range 0-255
hash :: Num b => ByteString -> b
hash = fromIntegral (BS.foldl (+) 0)