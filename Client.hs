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
import System.Random

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

type RemotePlayer = TVar (Map String (Integer, Integer))

newRmtPlayerMap :: Map String (Integer, Integer) -> IO RemotePlayer
newRmtPlayerMap = newTVarIO

addRmtPlayer :: String -> (Integer, Integer) -> TVar (Map String (Integer, Integer)) -> STM ()
addRmtPlayer id pos m = do
    map <- readTVar m
    let newmap = Data.Map.insert id pos map
    writeTVar m (Data.Map.union newmap map)

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

appLoop :: Player -> Handle -> TVar (Map String (Integer, Integer)) -> Renderer -> IO ()
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

  let pn = playerName player
  let pp = getMousePos mouseMotions (playerPos player)
  let ppx = view _x pp
  let ppy = view _y pp
  let nplayer = updatePlayer player pn (P(V2 ppx ppy))
  let py = view _y (playerPos player)
  let px = view _x (playerPos player)
  Char8.hPut handle $ Char8.append (Char8.pack $ playerName player) $ Char8.append "." $ Char8.append (Char8.pack $ show px) $ Char8.append "." $ Char8.append (Char8.pack $ show py) "!\n"
  clear renderer
  rendererDrawColor renderer $= V4 255 255 255 255
  fillRect renderer (Just (Rectangle (P (V2 0 0)) (V2 1024 800)))
  rendererDrawColor renderer $= V4 222 222 222 255
  forM_ [1 .. 32] $ \s -> do
    drawLine renderer (P (V2 (s*32) 0)) (P (V2 (s*32) 800))
    drawLine renderer (P (V2 0 (s*32))) (P (V2 1024 (s*32)))
  rmtPlayerMap <- readTVarIO rmtPlayer
  mapM_ (\(i, (x, y)) -> do
    rendererDrawColor renderer $= V4 33 255 66 255
    fillRect renderer (Just (Rectangle (P (V2 (fromIntegral $ snapping x) (fromIntegral $ snapping y))) (V2 32 32)))) (toList rmtPlayerMap)
  -- fillRect renderer (Just (Rectangle (fmap fromIntegral (playerPos player)-P (V2 16 16)) (V2 32 32)))
  rendererDrawColor renderer $= V4 0 0 0 255
  fillRect renderer (Just (Rectangle (P (V2 (fromIntegral $ snapping px) (fromIntegral $ snapping py))) (V2 32 32)))
  present renderer
  unless qPressed (appLoop nplayer handle rmtPlayer renderer)

snapping :: Integral p => p -> p
snapping x= div x 32 * 32

getMousePos :: Num b => [MouseMotionEventData] -> Point V2 b -> Point V2 b
getMousePos (x:xs) _ = fmap fromIntegral (mouseMotionEventPos  x)
getMousePos _ p = p

handleConnection :: Handle -> TVar (Map String (Integer, Integer))-> IO b
handleConnection handle rmtPlayer = do
  l <- hGetLine handle
  let id:x:y:xs = splitOn "." l
  let intid = Just id
  let intx = readMaybe x
  let inty = readMaybe y
  putStrLn ("id:" ++ id ++ " x:" ++ x ++ " y:" ++ y)
  case (intid, intx, inty) of
    (Just id, Just x, Just y) -> atomically $ addRmtPlayer id (x, y) rmtPlayer
    _ -> putStrLn "Parsing error"
  handleConnection handle rmtPlayer





