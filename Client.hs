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

writeS = do
	hdl <- Net.connectTo "0.0.0.0" (Net.PortNumber 9999)
	netWrite hdl . formatRequest $ Set "hello" "goodbye"
	hClose hdl

readS = do 
	hdl <- Net.connectTo "0.0.0.0" (Net.PortNumber 9999)
	netWrite hdl . formatRequest $ Get "hello"
	responseData <- netRead hdl
	let response = responseData >>= parseResponse
	case response of
		Left err -> error ("Response error: " ++ err)
		Right (Found "hello" "goodbye") -> return ()
		Right x -> error ("Bad response:" ++ show x)
	hClose hdl

main = Net.withSocketsDo . forever $ do
	writeS
	readS
	
	