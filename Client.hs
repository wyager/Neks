{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.KVStore.Message (formatRequests, parseResponses)
import Network.KVStore.NetPack (netWrite, netRead)
import Network.KVStore.Actions (Request(..), Reply(..))

import qualified Network as Net
import System.IO (Handle, hClose)
import Data.ByteString (ByteString)

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)

connectTo :: String -> IO Handle
connectTo server = Net.connectTo server (Net.PortNumber 9999)


main = Net.withSocketsDo $ do
	locks <- sequence [newEmptyMVar | _ <- [1..100]] -- 100 threads
	threads <- sequence [forkIO (testWith lock) | lock <- locks]
	sequence_ [takeMVar lock | lock <- locks] -- Wait for threads to finish

testWith :: MVar () -> IO ()
testWith lock = do
	server <- connectTo "0.0.0.0"
	sequence_ $ replicate 500 $ writeTo server >> readFrom server
	putMVar lock ()
	where
	writeTo server = do
		let requests = [Set k v | (k, v) <- zip testKeys testValues]
		netWrite server $ formatRequests requests
	readFrom server = do
		let requests = [Get key | key <- testKeys]
		netWrite server $ formatRequests requests
		responseData <- netRead server
		let responses = responseData >>= parseResponses
		if responses == Right [Found k v | (k, v) <- zip testKeys testValues]
			then return ()
			else error "Bad response"

testKeys = ["66991fb944", "afe0c0261a", "a4242d5dda", "d10db90845", "4384ecbfe", "a839702a82", "1ed8680b95", "0d2189d279", "f4b0795239", "a24d4e7e87", "28e24e1d51", "9bb0dfbfbd", "9776bad265", "89f79a8c71", "d50de7c1cd", "167a350f93", "36f41a6205", "f5bbd3bc20", "69a3d20bef", "33644bede7", "8744571558", "cd4ab79d3a", "8c26e6936c", "88c1d42e4e", "f31d532d05", "a9ad46aea2", "e9b0aeee64", "dffc6a25af", "90952b9dd", "04a136756e", "31ca38445e", "21c27b172", "5c09e01c46", "9b23b5ef27", "a9fd5ea170", "aa1718e735", "1ce6781a57", "a927b0584e", "e7aea00872", "52223f7078", "e620de282a", "1a4c71def8", "75bd1abc65", "af93442708", "2257127db4", "68ec4b4f7", "9b5473f839", "d453871c0f", "9657631a3d", "95503a22b9"]
testValues = ["5e7a195e90", "accdfc69c4", "43be950623", "afed0a6890", "0d23711bcf", "3b3d9b4043", "139ba09036", "a54b56630d", "61a729c150", "34891805ca", "d3dc68c9d3", "e1b4943d72", "8731015486", "f8f626c071", "4262ca1f24", "3c55632f50", "d32b8b30ca", "3311af7221", "29144d27ea", "0e0f97257e", "d6a2e1086", "aae1906c17", "d57f58433f", "9232138b5e", "fd1711214f", "84a66c50ac", "9b65ffc322", "d2d447396e", "6fc6c53265", "5183bca85", "884a5cc1cf", "7914d452ae", "6e2a351fd8", "7fb80954be", "3c3f1bf0cd", "112e60a719", "4917c12e1c", "9aaf5cc6d1", "7ccd97a418", "48c91da08c", "349524f781", "7d248047c", "9bfec0c3a4", "c0de587385", "216dd64a29", "eac5049f63", "133a259613", "843e1f1ee3", "e9c11331c0", "48e720933e"]