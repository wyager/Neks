module Network.KVStore.Message (
	parseRequests,
	formatRequests,
	parseResponses,
	formatResponses
) where

import Data.ByteString (ByteString)
import Network.KVStore.Actions (Request(Set, Get, Delete, Atomic), Reply(Found, NotFound))
import qualified Data.Serialize as Serialize
import Data.MessagePack (Object(ObjectArray, ObjectInt, ObjectBinary))
import Control.Applicative ((<$>))

formatRequests :: [Request] -> ByteString
formatRequests = Serialize.encode . ObjectArray . map format
	where format request = ObjectArray $ case request of
		Get k   		-> [ObjectInt 0, ObjectBinary k]
		Set k v 		-> [ObjectInt 1, ObjectBinary k, ObjectBinary v]
		Delete k 		-> [ObjectInt 2, ObjectBinary k]
		Atomic requests -> [ObjectInt 3, ObjectArray (map format requests)]

formatResponses :: [Reply] -> ByteString
formatResponses = Serialize.encode . ObjectArray . map (ObjectArray . format)
	where format response = case response of
		Found k v  -> [ObjectInt (-1), ObjectBinary k, ObjectBinary v]
		NotFound k -> [ObjectInt (-2), ObjectBinary k]


decode bs = case Serialize.decode bs of
	Right (ObjectArray messages) -> Right messages
	error -> Left "Response decode failure"

parseRequests :: ByteString -> Either String [Request]
parseRequests bs = decode bs >>= mapM parse where
	parse (ObjectArray [ObjectInt 0, ObjectBinary k]) = Right (Get k)
	parse (ObjectArray [ObjectInt 1, ObjectBinary k, ObjectBinary v]) = Right (Set k v)
	parse (ObjectArray [ObjectInt 2, ObjectBinary k]) = Right (Delete k)
	parse (ObjectArray [ObjectInt 3, ObjectArray requests]) = Atomic <$> mapM parse requests
	parse _ = Left "Invalid request structure"

parseResponses :: ByteString -> Either String [Reply]
parseResponses bs = decode bs >>= mapM parse where
	parse (ObjectArray [ObjectInt (-1), ObjectBinary k, ObjectBinary v]) = Right (Found k v)
	parse (ObjectArray [ObjectInt (-2), ObjectBinary k]) = Right (NotFound k)
	parse _ = Left "Incorrect response type"