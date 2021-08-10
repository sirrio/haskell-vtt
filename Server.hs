{-# LANGUAGE OverloadedStrings #-}
module Main where
import Network.Socket
    ( Socket,
      SockAddr(SockAddrInet),
      Family(AF_INET),
      SocketType(Stream),
      socketToHandle,
      setSocketOption,
      accept,
      bind,
      listen,
      socket,
      SocketOption(ReuseAddr) )
import System.IO
    ( hClose,
      hSetBuffering,
      hGetLine,
      hPutStrLn,
      BufferMode(NoBuffering),
      IOMode(ReadWriteMode) )
import Control.Exception ( SomeException(SomeException), handle )
import Control.Concurrent
    ( dupChan, newChan, readChan, writeChan, forkIO, killThread, Chan, threadDelay )
import Control.Monad (when)
import Control.Monad.Fix (fix)
import qualified System.IO as C

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 3000 0)
  listen sock 2
  chan <- newChan
  mainLoop sock chan 0

type Msg = (Int, String)

mainLoop :: Socket -> Chan Msg -> Int -> IO ()
mainLoop sock chan msgNum = do
  conn <- accept sock -- RUNS WHEN NEW CLIENT CONNECTS
  forkIO $ runConn conn chan msgNum
  mainLoop sock chan $! msgNum + 1

runConn :: (Socket, SockAddr) -> Chan Msg -> Int -> IO ()
runConn (sock, _) chan msgNum = do
    let broadcast msg = writeChan chan (msgNum, msg)
    hdl <- socketToHandle sock ReadWriteMode
    hSetBuffering hdl NoBuffering
    name <- fmap init (hGetLine hdl)
    putStrLn (name ++ " connected to the server! Player assigned ID " ++ show msgNum)
    -- hPutStrLn hdl name

    commLine <- dupChan chan

    reader <- forkIO $ fix $ \loop -> do
        (nextNum, line) <- readChan commLine
        when (msgNum /= nextNum) $ hPutStrLn hdl line
        loop

    handle (\(SomeException _) -> return ()) $ fix $ \loop -> do
        line <- fmap init (hGetLine hdl)
        -- threadDelay 1000 
        broadcast  line
        -- putStrLn line
        loop

    killThread reader
    hClose hdl