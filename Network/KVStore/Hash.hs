module Network.KVStore.Hash (
	hash
) where

import Data.ByteString as BS (ByteString, foldl)
import Data.Bits ((.&.))

-- Produces a number in the range 0-31
hash :: Num b => ByteString -> b
hash = fromIntegral . (.&. 0x1F) . (BS.foldl (+) 0)