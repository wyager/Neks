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

parseRequests :: ByteString -> Either String [Request]
parseRequests bs = case decode bs of
	Right (ObjectArray requests) -> sequence (map parse requests)
	Left error -> Left ("Request decode error: " ++ error)
	where parse request = case request of
		ObjectArray [ObjectInt 0, ObjectBinary k] -> Right (Get k)
		ObjectArray [ObjectInt 1, ObjectBinary k, ObjectBinary v] -> Right (Set k v)
		x -> Left ("Invalid request structure: " ++ show x)

formatRequests :: [Request] -> ByteString
formatRequests = encode . ObjectArray . map (ObjectArray . format)
	where format request = case request of
		Set k v -> [ObjectInt 1, ObjectBinary k, ObjectBinary v]
		Get k   -> [ObjectInt 0, ObjectBinary k]

parseResponses :: ByteString -> Either String [Reply]
parseResponses bs = case decode bs of
	Right (ObjectArray responses) -> sequence (map parse responses)
	Left error -> Left ("Response decode error: " ++ error)
	where parse response = case response of
		ObjectArray [ObjectInt (-1), ObjectBinary k, ObjectBinary v] -> Right (Found k v)
		ObjectArray [ObjectInt (-2), ObjectBinary k] -> Right (NotFound k)
		_ -> Left "Incorrect response type"

formatResponses :: [Reply] -> ByteString
formatResponses = encode . ObjectArray . map (ObjectArray . format)
	where format response = case response of
		Found k v  -> [ObjectInt (-1), ObjectBinary k, ObjectBinary v]
		NotFound k -> [ObjectInt (-2), ObjectBinary k]