module Main where

import qualified Network as Net
import System.IO (Handle, hClose)
import System.Environment (getArgs)
import Data.ByteString (ByteString)
import Control.Monad (forever, unless)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent (ThreadId, forkIO, threadDelay)
import Control.Exception (SomeException, catch, finally) 
import Network.Neks.Disk (saveTo, loadFrom)
import Network.Neks.NetPack (netRead, netWrite)
import Network.Neks.Message (parseRequests, formatResponses)
import Network.Neks.DataStore (DataStore, createStore, insert, get, delete)
import Network.Neks.Actions (Request(Set, Get, Delete, Atomic), Reply(Found, NotFound))

type Store = DataStore ByteString ByteString

main = do
        args <- getArgs
        case args of
                [] -> serveWithPersistence
                ["--no-persistence"] -> serveWithoutPersistence
                ["--help"] -> putStrLn instructions -- To be explicit
                _          -> putStrLn instructions

serveWithPersistence = do
        loaded <- loadFrom "store.kvs"
        globalStore <- case loaded of
                Just store -> return store
                Nothing -> atomically createStore
        let periodically action = forever (threadDelay (30*10^6) >> action)
        forkIO $ periodically (saveTo "store.kvs" globalStore)
        putStrLn "Serving on port 9999 \nSaving DB to store.kvs"
        serve globalStore (Net.PortNumber 9999)

serveWithoutPersistence = do
        globalStore <- atomically createStore
        putStrLn "Serving on port 9999\nNot persisting to disk"
        serve globalStore (Net.PortNumber 9999)

serve :: Store -> Net.PortID -> IO ()
serve store port = Net.withSocketsDo $ do
        sock <- Net.listenOn port
        forever (wait sock store)

wait :: Net.Socket -> Store -> IO ThreadId
wait sock store = do
        (client, _, _) <- Net.accept sock
        forkIO (run client)
        where
        run client = (handle client store `finally` hClose client) `catch` exceptionHandler
        exceptionHandler :: SomeException -> IO ()
        exceptionHandler exception = return () -- Optional exception reporting

handle :: Handle -> Store -> IO ()
handle client store = do
        result <- processRequests client store
        case result of
                Right success -> handle client store
                Left failure -> return () -- Optional soft error reporting

processRequests :: Handle -> Store -> IO (Either String ())
processRequests client store = do
        requestData <- netRead client
        case requestData >>= parseRequests of
                Left err -> return (Left err)
                Right requests -> do
                        results <- mapM (atomically . processWith store) requests
                        netWrite client . formatResponses . concat $ results
                        return (Right ())

processWith :: Store -> Request -> STM [Reply]
processWith store (Set k v) = do
        insert k v store
        return []
processWith store (Get k) = do
        result <- get k store
        return $ case result of
                Nothing -> [NotFound]
                Just v -> [Found v]
processWith store (Delete k) = do
        delete k store
        return []
processWith store (Atomic requests) = do
        results <- mapM (processWith store) requests
        return (concat results)

instructions = "Usage: NeksServer <opt-args>\n" ++
               "<opt-args> can be empty or can be \"--no-persistence\" to disable storing keys and values on disk"
