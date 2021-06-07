{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString.Char8 as C
import Network.Socket.ByteString (recv, sendAll)
import Network.Run.TCP (runTCPClient)

main :: IO ()
main = runTCPClient "127.0.0.1" "3000" init
    where
        init s = do
            putStrLn "Type your name:"
            name <- C.getLine
            talk s name
        talk s name = do
            putStrLn "Type messege: "
            msg <- C.getLine 
            sendAll s (C.append (C.append name ": ") msg)
            msg <- recv s 1024
            putStr ">"
            C.putStrLn msg
            talk s name
