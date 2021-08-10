{-# LANGUAGE OverloadedStrings #-}
module Main where
import Network.Socket
    ( socketToHandle, getAddrInfo, withSocketsDo, connect, socket, defaultProtocol, AddrInfo(addrFamily, addrAddress), SocketType(Stream) )
import System.IO
import qualified Data.ByteString.Char8 as Char8
import SDL
import Linear (V2(..))
import Control.Monad(void, unless, when, forM_)
import Control.Monad.IO.Class(MonadIO (liftIO))
import Data.Text(Text, empty)
import Foreign.C.Types(CInt)
import Data.List
import Data.Maybe
import Control.Concurrent
    ( dupChan, newChan, readChan, writeChan, forkIO, killThread, Chan, getChanContents )
import Data.ByteString.Lazy (toStrict, fromStrict)
import Control.Lens
import Data.Map
import Control.Monad.Fix (fix)
import Data.List.Split
import Control.Concurrent.STM
import Text.Read (readMaybe)

data Player = Player {
  playerPos :: Point V2 Integer,
  playerName :: String
}

initialPlayer :: Player
initialPlayer = Player {
  playerPos = P (V2 0 0),
  playerName = "default"
}

updatePlayer :: Player -> String -> Point V2 Integer -> Player
updatePlayer w n p = w { playerPos = p,  playerName = n }

type RemotePlayer = TVar (Map Integer (Integer, Integer))

newRmtPlayerMap :: Map Integer (Integer, Integer) -> IO RemotePlayer
newRmtPlayerMap = newTVarIO

addRmtPlayer :: Integer -> (Integer, Integer) -> TVar (Map Integer (Integer, Integer)) -> STM ()
addRmtPlayer id pos m = do
    map <- readTVar m
    let newmap = Data.Map.insert id pos map
    writeTVar m newmap

main :: IO ()
main = do
  initializeAll
  addrInfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "3000")
  socket <- socket (addrFamily (head addrInfos)) Stream defaultProtocol
  connect socket (addrAddress (head addrInfos))
  handle <- socketToHandle socket ReadWriteMode
  hSetBuffering handle NoBuffering
  putStrLn "Connection estabished: choose a name!"
  playername <- Char8.getLine
  Char8.hPut handle (Char8.append playername "s\n")
  let nplayer = updatePlayer initialPlayer (Char8.unpack playername) (playerPos initialPlayer)
  rmtPlayer <- newRmtPlayerMap Data.Map.empty
  window <- createWindow "VTT Haskell" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  forkIO $ handleConnection handle rmtPlayer
  appLoop nplayer handle rmtPlayer renderer
  destroyWindow window

appLoop :: Player -> Handle -> TVar (Map Integer (Integer, Integer)) -> Renderer -> IO ()
appLoop player handle rmtPlayer renderer = do
  events <- pollEvents
  let eventIsQPress event =
        case eventPayload event of
          KeyboardEvent keyboardEvent ->
            keyboardEventKeyMotion keyboardEvent == Pressed &&
            keysymKeycode (keyboardEventKeysym keyboardEvent) == KeycodeQ
          _ -> False
      qPressed = any eventIsQPress events
  let mousePressedEvent event =
        case eventPayload event of
          MouseButtonEvent mouseButtonEvent ->
            mouseButtonEventMotion mouseButtonEvent == Pressed
          _ -> False
      mousePressed = any mousePressedEvent events
  let mouseMotionEventMap event =
        case eventPayload event of
          MouseMotionEvent mme -> Just mme
          _ -> Nothing
      mouseMotions = Data.Maybe.mapMaybe mouseMotionEventMap events

  let nplayer = updatePlayer player (playerName player) (getMousePos mouseMotions (playerPos player))
  Char8.hPut handle $ Char8.append (Char8.pack $ show (view _x (playerPos player))) $ Char8.append "." $ Char8.append (Char8.pack $ show (view _y (playerPos player))) "!\n"

  clear renderer
  rendererDrawColor renderer $= V4 133 133 133 255
  fillRect renderer (Just (Rectangle (P (V2 0 0)) (V2 1024 800)))

  rmtPlayerMap <- readTVarIO rmtPlayer
  mapM_ (\(i, (x, y)) -> do
    rendererDrawColor renderer $= V4 (fromIntegral i+1 `mod` 255) (fromIntegral (i+1)*25 `mod` 255) (fromIntegral (i+1)*50 `mod` 255) 255
    fillRect renderer (Just (Rectangle (P (V2 (fromIntegral x) (fromIntegral y))) (V2 32 32)))) (toList rmtPlayerMap)
  rendererDrawColor renderer $= V4 0 0 0 255
  fillRect renderer (Just (Rectangle (fmap fromIntegral (playerPos player)-P (V2 16 16)) (V2 32 32)))
  present renderer
  unless qPressed (appLoop nplayer handle rmtPlayer renderer)

getMousePos :: Num b => [MouseMotionEventData] -> Point V2 b -> Point V2 b
getMousePos (x:xs) _ = fmap fromIntegral (mouseMotionEventPos  x)
getMousePos _ p = p


handleConnection :: Handle -> TVar (Map Integer (Integer, Integer))-> IO b
handleConnection handle rmtPlayer = do
  l <- hGetLine handle
  let id:x:y:xs = splitOn "." l
  let intid = readMaybe id
  let intx = readMaybe x
  let inty = readMaybe y
  putStrLn ("id:" ++ id ++ " x:" ++ x ++ " y:" ++ y)
  case (intid, intx, inty) of
    (Just id, Just x, Just y) -> atomically $ addRmtPlayer id (x, y) rmtPlayer
    _ -> putStrLn "Parsing error"
  handleConnection handle rmtPlayer





