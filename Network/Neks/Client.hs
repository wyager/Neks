{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Neks.Message (formatRequests, parseResponses)
import Network.Neks.NetPack (netWrite, netRead)
import Network.Neks.Actions (Request(Set, Get, Delete, Atomic), Reply(Found, NotFound))

import qualified Network as Net
import System.IO (Handle)
import System.Environment (getArgs)
import Data.ByteString.Char8 (pack)

import Control.Monad (when)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)

main = Net.withSocketsDo $ do
        args <- getArgs
        case args of
                (host:port:command) -> do 
                        let portID = Net.PortNumber . fromInteger . read $ port
                        server <- Net.connectTo host portID
                        case command of
                                ["--set", k, v] -> set k v server
                                ["--get", k]    -> get k server
                                ["--test"]      -> test host portID
                                _               -> putStrLn instructions
                ["--help"] -> putStrLn instructions -- To be explicit
                _          -> putStrLn instructions

request :: Handle -> [Request] -> IO (Either String [Reply])
request server requests = do
        netWrite server $ formatRequests requests
        responseData <- netRead server
        return (responseData >>= parseResponses)

set :: String -> String -> Handle -> IO ()
set k v server = do
        response <- request server [Set (pack k) (pack v)]
        case response of
                Left error -> putStrLn ("Error setting value: " ++ error)
                Right [] -> putStrLn "Set successful"

get :: String -> Handle -> IO ()
get k server = do
        response <- request server [Get (pack k)]
        case response of
                Left error -> putStrLn ("Error getting value: " ++ error)
                Right response -> putStrLn ("Response received: " ++ show response)

test :: String -> Net.PortID -> IO ()
test host port = do
        putStrLn "Spawning 50 threads x 200 requests x 100 transactions"
        locks <- sequence [newEmptyMVar | _ <- [1..50]] -- 50 threads
        threads <- sequence [forkIO (testWith lock host port) | lock <- locks]
        sequence_ [takeMVar lock | lock <- locks] -- Wait for threads to finish
        putStrLn "Test complete"
        where testWith lock host port = do
                server <- Net.connectTo host port
                sequence . replicate 200 $ do
                        let requests = [Set k v | (k, v) <- zip testKeys testValues] ++ [Get k | k <- testKeys]
                        responses <- request server requests
                        when (responses /= Right [Found v | v <- testValues]) (error "Bad response")
                putMVar lock ()

testKeys = ["66991fb944", "afe0c0261a", "a4242d5dda", "d10db90845", "4384ecbfe", "a839702a82", "1ed8680b95", "0d2189d279", "f4b0795239", "a24d4e7e87", "28e24e1d51", "9bb0dfbfbd", "9776bad265", "89f79a8c71", "d50de7c1cd", "167a350f93", "36f41a6205", "f5bbd3bc20", "69a3d20bef", "33644bede7", "8744571558", "cd4ab79d3a", "8c26e6936c", "88c1d42e4e", "f31d532d05", "a9ad46aea2", "e9b0aeee64", "dffc6a25af", "90952b9dd", "04a136756e", "31ca38445e", "21c27b172", "5c09e01c46", "9b23b5ef27", "a9fd5ea170", "aa1718e735", "1ce6781a57", "a927b0584e", "e7aea00872", "52223f7078", "e620de282a", "1a4c71def8", "75bd1abc65", "af93442708", "2257127db4", "68ec4b4f7", "9b5473f839", "d453871c0f", "9657631a3d", "95503a22b9"]
testValues = ["5e7a195e90", "accdfc69c4", "43be950623", "afed0a6890", "0d23711bcf", "3b3d9b4043", "139ba09036", "a54b56630d", "61a729c150", "34891805ca", "d3dc68c9d3", "e1b4943d72", "8731015486", "f8f626c071", "4262ca1f24", "3c55632f50", "d32b8b30ca", "3311af7221", "29144d27ea", "0e0f97257e", "d6a2e1086", "aae1906c17", "d57f58433f", "9232138b5e", "fd1711214f", "84a66c50ac", "9b65ffc322", "d2d447396e", "6fc6c53265", "5183bca85", "884a5cc1cf", "7914d452ae", "6e2a351fd8", "7fb80954be", "3c3f1bf0cd", "112e60a719", "4917c12e1c", "9aaf5cc6d1", "7ccd97a418", "48c91da08c", "349524f781", "7d248047c", "9bfec0c3a4", "c0de587385", "216dd64a29", "eac5049f63", "133a259613", "843e1f1ee3", "e9c11331c0", "48e720933e"]

instructions = "Usage: Client <host> <port> <args>\n" ++
                           "<args> are \"--test\", \"--get <key>\", or \"--set <key> <value>\""
