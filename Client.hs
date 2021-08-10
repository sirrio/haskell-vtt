{-# LANGUAGE OverloadedStrings #-}
module Main where
import Network.Socket
    ( socketToHandle,
      getAddrInfo,
      withSocketsDo,
      connect,
      socket,
      defaultProtocol,
      AddrInfo(addrFamily, addrAddress),
      SocketType(Stream),
      Socket, sendBuf )
import System.IO
    ( hSetBuffering,
      hGetLine,
      BufferMode(BlockBuffering, NoBuffering, LineBuffering),
      IOMode(ReadWriteMode),
      Handle, hFlush, char8)
import qualified Data.ByteString.Char8 as Char8
import GHC.IO.Handle (hFlushAll)
import SDL
import Linear (V4(..))
import Control.Monad(void, unless, when, forM_)
import Control.Monad.IO.Class(MonadIO (liftIO))
import Data.Text(Text, empty)
import Foreign.C.Types(CInt)
import SDL.Input.Mouse (getRelativeMouseLocation)
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

data World = World {
  playerPos :: Point V2 Integer,
  playerName :: String
}

type RemotePlayer = TVar (Map Integer (Integer, Integer))


newRmtPlayerMap :: Map Integer (Integer, Integer) -> IO RemotePlayer
newRmtPlayerMap = newTVarIO

addRmtPlayer :: Integer -> (Integer, Integer) -> TVar (Map Integer (Integer, Integer)) -> STM ()
addRmtPlayer id pos m = do
    map <- readTVar m
    let newmap = Data.Map.insert id pos map
    writeTVar m newmap

-- newRP :: [(String, Integer, Integer)] 

-- data RemotePlayer = RemotePlayer { rpList :: Map String (Point V2 Integer) }

initialWorld :: World
initialWorld = World {
  playerPos = P (V2 0 0),
  playerName = "default"
}

-- initialRemotePlayer :: RemotePlayer 
-- initialRemotePlayer = RemotePlayer {
--   rpList = Data.Map.empty
-- } 

updateWorld :: World -> String -> Point V2 Integer -> World
updateWorld w n p = w { playerPos = p,  playerName = n }

type Msg = (Int, String)

main :: IO ()
main = do
  initializeAll
  rmtPlayer <- newRmtPlayerMap Data.Map.empty
  addrInfos <- getAddrInfo Nothing (Just "127.0.0.1") (Just "3000")
  socket <- socket (addrFamily (head addrInfos)) Stream defaultProtocol
  connect socket (addrAddress (head addrInfos))
  handle <- socketToHandle socket ReadWriteMode
  hSetBuffering handle NoBuffering
  putStrLn "Connection estabished: choose a name!"
  playername <- Char8.getLine
  Char8.hPut handle (Char8.append playername "s\n")
  let updatedWorld = updateWorld initialWorld (Char8.unpack playername) (playerPos initialWorld)
  -- let remotePlayer = Data.Map.empty
  -- chan <- newChan
  window <- createWindow "VTT Haskell" defaultWindow
  renderer <- createRenderer window (-1) defaultRenderer
  forkIO $ handleConnection handle rmtPlayer
  appLoop updatedWorld handle rmtPlayer renderer
  destroyWindow window

appLoop :: World -> Handle -> TVar (Map Integer (Integer, Integer)) -> Renderer -> IO ()
appLoop world handle rmtPlayer renderer = do
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

  let w = updateWorld world (playerName world) (getMousePos mouseMotions (playerPos world))
  -- Char8.hPut handle $ Char8.append (Char8.pack $ playerName world) $ Char8.append "." $ Char8.append (Char8.pack $ show (view _x (playerPos world))) $ Char8.append "." $ Char8.append (Char8.pack $ show (view _y (playerPos world))) "\n"
  -- Char8.hPut handle (Char8.append (serialize (playerName world) (playerPos world)) "s\n")
  Char8.hPut handle $ Char8.append (Char8.pack $ show (view _x (playerPos world))) $ Char8.append "." $ Char8.append (Char8.pack $ show (view _y (playerPos world))) "\n"

  -- if mouseMoved then putStrLn "TRUE" else putStrLn "False"
  -- if fmap (payloadToIntent . SDL.eventPayload) == Hover TopLeft then putStrLn "yes" else putStrLn "not"    
    -- forM_ mouseMotions $ \s -> do
  --     let lastPos = fmap fromIntegral (mouseMotionEventPos  s)
  --     liftIO w <- updateWorld world (P (V2 15 15))
  --     print lastPos

  -- reader <- forkIO $ fix $ \loop -> do
  --   -- (_, line):list <- getChanContents commLine
  --   ( _, line) <- readChan chan
  --   putStrLn line
  --   loop

    -- let name:x:y:xs = splitOn "x" line
    -- putStrLn ("X: " ++ x)
    -- putStrLn ("Y: " ++ y)
    -- fillRect renderer (Just (Rectangle (P (V2 (read x) (read y))) (V2 32 32)))

      -- fillRect renderer (Just (Rectangle (P (V2 (read x) (read y))) (V2 32 32)))
  -- rendererDrawColor renderer $= V4 255 0 0 255
  -- fillRect renderer (Just (Rectangle (P (V2 50 50)) (V2 32 32)))

  clear renderer
  rendererDrawColor renderer $= V4 133 133 133 255
  fillRect renderer (Just (Rectangle (P (V2 0 0)) (V2 1024 800)))

  rendererDrawColor renderer $= V4 255 0 0 255
  rmtPlayerMap <- readTVarIO rmtPlayer
  mapM_ (\(_,(x, y)) ->   fillRect renderer (Just (Rectangle (P (V2 (fromIntegral x) (fromIntegral y))) (V2 32 32)))) (toList rmtPlayerMap)

  rendererDrawColor renderer $= V4 255 0 0 255
  fillRect renderer (Just (Rectangle (P (V2 50 50)) (V2 32 32)))
  rendererDrawColor renderer $= V4 0 0 0 255
  fillRect renderer (Just (Rectangle (fmap fromIntegral (playerPos world)-P (V2 16 16)) (V2 32 32)))
  present renderer
  unless qPressed (appLoop w handle rmtPlayer renderer)


-- serialize :: p -> Point V2 Integer -> Char8.ByteString
-- serialize name location = toStrict $ encode(42 :: Integer)
-- -- serialize name location = "helloworld"


getMousePos :: Num b => [MouseMotionEventData] -> Point V2 b -> Point V2 b
getMousePos (x:xs) _ = fmap fromIntegral (mouseMotionEventPos  x)
getMousePos _ p = p


handleConnection :: Handle -> TVar (Map Integer (Integer, Integer))-> IO b
handleConnection handle rmtPlayer = do
  l <- hGetLine handle
  -- putStrLn l
  let id:x:y:xs = splitOn "." l
  let intid = readMaybe id
  let intx = readMaybe x
  let inty = readMaybe y
  case (intid, intx, inty) of
    (Just id, Just x, Just y) -> atomically $ addRmtPlayer id (x, y) rmtPlayer
    _ -> putStrLn "Parsing error"

  -- let map = Data.Map.insert "1" (P (V2 0 0)) Data.Map.empty
  -- let broadcast msg = writeChan chan (0, msg)
  -- broadcast l
  handleConnection handle rmtPlayer





