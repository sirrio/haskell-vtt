{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C
import Network.Run.TCP (runTCPServer)
import Network.Socket.ByteString (recv, sendAll)

data TurnstileSate = Locked | Unlocked
  deriving (Eq, Show)

main :: IO ()
main = runTCPServer Nothing "3000" talk
  where
    talk s = do
        msg <- recv s 1024
        unless (S.null msg) $ do
          sendAll s msg
          C.putStrLn msg
          talk s