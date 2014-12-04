module Network.KVStore.Message (
	parseRequest,
	formatRequest,
	parseResponse,
	formatResponse
) where

import Data.ByteString (ByteString)
import Network.KVStore.Actions (Request(Set, Get), Reply(Found, NotFound))
import Data.Serialize (encode, decode)
import Data.MessagePack (Object(ObjectArray, ObjectInt, ObjectBinary))

parseRequest :: ByteString -> Either String Request
parseRequest bs = case decode bs of
	Right (ObjectArray [ObjectInt 0, ObjectBinary k]) -> Right (Get k)
	Right (ObjectArray [ObjectInt 1, ObjectBinary k, ObjectBinary v]) -> Right (Set k v)
	Right x -> Left ("Invalid request structure: " ++ show x)
	Left error -> Left ("Request decode error: " ++ error)

formatRequest :: Request -> ByteString
formatRequest (Set k v) = encode $ ObjectArray [ObjectInt 1,
												ObjectBinary k,
												ObjectBinary v]
formatRequest (Get k) 	= encode $ ObjectArray [ObjectInt 0,
												ObjectBinary k]

parseResponse :: ByteString -> Either String Reply
parseResponse response = case decode response of
	Right (ObjectArray [ObjectInt (-1), ObjectBinary k, ObjectBinary v]) -> Right (Found k v)
	Right (ObjectArray [ObjectInt (-2), ObjectBinary k]) -> Right (NotFound k)
	Right _ -> Left "Incorrect response type"
	Left error -> Left ("Response decode error: " ++ error)

formatResponse :: Reply -> ByteString
formatResponse (Found k v)  = encode $ ObjectArray [ObjectInt (-1),
												  	ObjectBinary k,
												  	ObjectBinary v]
formatResponse (NotFound k) = encode $ ObjectArray [ObjectInt (-2),
													ObjectBinary k]
