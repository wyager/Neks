{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.KVStore.Message (formatRequest, parseResponse)
import Network.KVStore.NetPack (netWrite, netRead)
import Network.KVStore.Actions (Request(..), Reply(..))
import qualified Data.ByteString.Lazy as BSL

import qualified Network as Net
import System.IO (Handle, hClose)

import Control.Monad (forever)
import Control.Applicative ((<$>))
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)



writeS = do
	hdl <- Net.connectTo "0.0.0.0" (Net.PortNumber 9999)
	let writeKV k v = netWrite hdl . formatRequest $ Set k v
	sequence $ map (\(k,v) -> writeKV k v) $ zip keys values
	hClose hdl

readS = do 
	hdl <- Net.connectTo "0.0.0.0" (Net.PortNumber 9999)
	let readK k v = do
		netWrite hdl . formatRequest $ Get k
		responseData <- netRead hdl
		let response = responseData >>= parseResponse
		case response of
			Left err -> error ("Response error: " ++ err)
			Right (Found k' v') -> if k' == k && v' == v
				then return ()
				else error "Value mismatch"
			Right x -> error ("Bad response:" ++ show x)
	sequence $ map (\(k,v) -> readK k v) $ zip keys values
	hClose hdl

main = Net.withSocketsDo $ do
	locks <- sequence [newEmptyMVar | _ <- [1..1000]]
	threads <- sequence [forkIO (connectWith lock) >> takeMVar lock | lock <- locks]
	--sequence_ [takeMVar lock | lock <- locks]
	return ()
	where connectWith lock = do
		writeS
		readS
		putMVar lock ()

keys = ["66991fb944", "afe0c0261a", "a4242d5dda", "d10db90845", "4384ecbfeb", "a839702a82", "1ed8680b95", "0d2189d279", "f4b0795239", "a24d4e7e87", "28e24e1d51", "9bb0dfbfbd", "9776bad265", "89f79a8c71", "d50de7c1cd", "167a350f93", "36f41a6205", "f5bbd3bc20", "69a3d20bef", "33644bede7", "8744571558", "cd4ab79d3a", "8c26e6936c", "88c1d42e4e", "f31d532d05", "a9ad46aea2", "e9b0aeee64", "dffc6a25af", "90952b9ddb", "04a136756e", "31ca38445e", "21c27b172b", "5c09e01c46", "9b23b5ef27", "a9fd5ea170", "aa1718e735", "1ce6781a57", "a927b0584e", "e7aea00872", "52223f7078", "e620de282a", "1a4c71def8", "75bd1abc65", "af93442708", "2257127db4", "68ec4b4f7b", "9b5473f839", "d453871c0f", "9657631a3d", "95503a22b9"]
values = ["5e7a195e90", "accdfc69c4", "43be950623", "afed0a6890", "0d23711bcf", "3b3d9b4043", "139ba09036", "a54b56630d", "61a729c150", "34891805ca", "d3dc68c9d3", "e1b4943d72", "8731015486", "f8f626c071", "4262ca1f24", "3c55632f50", "d32b8b30ca", "3311af7221", "29144d27ea", "0e0f97257e", "d6a2e1086b", "aae1906c17", "d57f58433f", "9232138b5e", "fd1711214f", "84a66c50ac", "9b65ffc322", "d2d447396e", "6fc6c53265", "5183bca85b", "884a5cc1cf", "7914d452ae", "6e2a351fd8", "7fb80954be", "3c3f1bf0cd", "112e60a719", "4917c12e1c", "9aaf5cc6d1", "7ccd97a418", "48c91da08c", "349524f781", "7d248047cb", "9bfec0c3a4", "c0de587385", "216dd64a29", "eac5049f63", "133a259613", "843e1f1ee3", "e9c11331c0", "48e720933e"]
