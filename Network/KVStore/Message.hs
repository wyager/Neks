module Network.KVStore.Message (
	parseRequests,
	formatRequests,
	parseResponses,
	formatResponses
) where

import Data.ByteString (ByteString)
import Network.KVStore.Actions (Request(Set, Get), Reply(Found, NotFound))
import Data.Serialize (encode, decode)
import Data.MessagePack (Object(ObjectArray, ObjectInt, ObjectBinary))

formatRequests :: [Request] -> ByteString
formatRequests = encode . ObjectArray . map (ObjectArray . format)
	where format request = case request of
		Set k v -> [ObjectInt 1, ObjectBinary k, ObjectBinary v]
		Get k   -> [ObjectInt 0, ObjectBinary k]

formatResponses :: [Reply] -> ByteString
formatResponses = encode . ObjectArray . map (ObjectArray . format)
	where format response = case response of
		Found k v  -> [ObjectInt (-1), ObjectBinary k, ObjectBinary v]
		NotFound k -> [ObjectInt (-2), ObjectBinary k]

parseRequests :: ByteString -> Either String [Request]
parseRequests = parse request
request (ObjectArray [ObjectInt 0, ObjectBinary k]) = Right (Get k)
request (ObjectArray [ObjectInt 1, ObjectBinary k, ObjectBinary v]) = Right (Set k v)
request _ = Left "Invalid request structure"

parseResponses :: ByteString -> Either String [Reply]
parseResponses = parse response
response (ObjectArray [ObjectInt (-1), ObjectBinary k, ObjectBinary v]) = Right (Found k v)
response (ObjectArray [ObjectInt (-2), ObjectBinary k]) = Right (NotFound k)
response _ = Left "Incorrect response type"

parse :: (Object -> Either String a) -> ByteString -> Either String [a]
parse parser bs = case decode bs of
	Right (ObjectArray responses) -> sequence (map parser responses)
	error -> Left "Response decode failure"