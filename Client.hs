{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (recv, sendAll)
import Network.Run.TCP (runTCPClient)

main :: IO ()
main = runTCPClient "127.0.0.1" "3000" $ \s -> do
    sendAll s "Hello, world!"
    msg <- recv s 1024
    putStr "Received: "
    C.putStrLn msg
