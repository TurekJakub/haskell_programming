{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent
import Control.Lens
import Control.Monad (forever, when)
import Control.Monad.State
import Data.IORef
import Data.Maybe (fromMaybe)
import Data.Set qualified as S
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import LogoProtocol
import Network.Socket
import Options.Applicative
import System.IO
import Text.Read (readMaybe)

data ClientState = ClientState
  { _canvasState :: St
  , _coverageRef :: IORef [CoverageInfo]
  , _pushChan :: Chan InMsg
  }

data CmdArgs = CmdArgs
  { _address :: String
  , _port :: String
  }

makeLenses ''Tile

makePrisms ''St

makeLenses ''ClientState

addCx :: (Num a, Num b) => (a, b) -> (a, b) -> (a, b)
addCx (a, b) (c, d) = (a + c, b + d)

succBnd :: (Eq a, Bounded a, Enum a) => a -> a
succBnd c
  | c == maxBound = minBound
  | otherwise = succ c

flipVShard :: Shard -> Shard
flipVShard N = S
flipVShard S = N
flipVShard x = x

rotL :: Tile -> Tile
rotL = over tileData (map $ \((x, y), ss) -> ((negate y, x), map succBnd ss))

rotR :: Tile -> Tile
rotR = rotL . rotL . rotL

flipV :: Tile -> Tile
flipV =
  over tileData (map $ \((x, y), ss) -> ((x, negate y), map flipVShard ss))

flipH :: Tile -> Tile
flipH = rotL . flipV . rotR

startingTiles :: [Tile]
startingTiles =
  [ Tile (0, 0) [((0, 0), [S, E, N, W])]
  , Tile (1, 0) [((0, 0), [N, W])]
  , Tile (1, 0) [((0, 0), [S, E]), ((1, 0), [N, W])]
  , Tile (2, 0) [((0, 0), [S, E]), ((1, 0), [S, W])]
  , Tile (3, 0) [((0, 0), [E, N])]
  , Tile (4, 0) [((0, 0), [S, E, N, W])]
  ]

cplx ::
     (Integral a1, Integral a2, Num t1, Num t2)
  => (t1 -> t2 -> t3)
  -> (a1, a2)
  -> t3
cplx f (a, b) = f (fromIntegral a) (fromIntegral b)

unSq :: p -> p -> p -> p -> Shard -> p
unSq s _ _ _ S = s
unSq _ e _ _ E = e
unSq _ _ n _ N = n
unSq _ _ _ w W = w

drawSq :: [Shard] -> Picture
drawSq =
  Pictures
    . map
        (unSq
           (Polygon [(0, 0), (1, 0), (0.5, 0.5)])
           (Polygon [(1, 0), (1, 1), (0.5, 0.5)])
           (Polygon [(1, 1), (0, 1), (0.5, 0.5)])
           (Polygon [(0, 1), (0, 0), (0.5, 0.5)]))

renderTile :: Tile -> Picture
renderTile (Tile pos subs) =
  cplx Translate pos
    $ Pictures [cplx Translate spos $ drawSq sq | (spos, sq) <- subs]

render :: ClientState -> IO Picture
render cs = do
  serverStats <- readIORef (cs ^. coverageRef)
  let confirmedCoords = S.fromList [(x, y) | (x, y, _, _) <- serverStats]
  let coverageStats =
        Pictures
          [ cplx Translate (x, y)
            $ Color
                (withAlpha
                   (if isMaj
                      then 0.75
                      else 0.5)
                   black)
            $ drawSq [sh]
          | (x, y, sh, isMaj) <- serverStats
          ]
  let st = cs ^. canvasState
      activeTile = (st ^? _Selecting . _1) <|> (st ^? _Dragging . _1)
      inactiveTiles =
        fromMaybe [] $ (st ^? _Selecting . _2) <|> (st ^? _Dragging . _2)
  let isConfirmed tPos (sPos, _) = S.member (addCx tPos sPos) confirmedCoords
  let renderUserTile t =
        let tPos = t ^. tilePos
         in Pictures
              [ translate (fromIntegral sx) (fromIntegral sy)
                $ Color
                    (if isConfirmed tPos sub
                       then dark red
                       else withAlpha 0.5 red)
                    (drawSq shards)
              | sub@((sx, sy), shards) <- t ^. tileData
              ]
  let userTiles =
        Pictures
          [ Pictures
              [ cplx Translate (t ^. tilePos) (renderUserTile t)
              | t <- inactiveTiles
              ]
          , maybe Blank (Color red . renderTile) activeTile
          ]
  pure $ Scale 100 100 $ Pictures [coverageStats, userTiles]

event :: Event -> ClientState -> IO ClientState
event ev cs = do
  let nextCs =
        case ev of
          EventKey (SpecialKey k) Down _ _ -> over canvasState (skEvent k) cs
          EventKey (Char c) Down _ _ -> over canvasState (ltrEvent c) cs
          _ -> cs
  when (cs ^. canvasState . to show /= nextCs ^. canvasState . to show)
    $ notifyServer nextCs
  pure nextCs

skEvent :: SpecialKey -> St -> St
skEvent k =
  execState
    $ case k of
        KeyTab ->
          zoom
            _Selecting
            do
              t <- use _1
              ts <- use _2
              case ts ++ [t] of
                (t':ts') -> do
                  _1 .= t'
                  _2 .= ts'
                [] -> pure ()
        KeySpace ->
          modify
            \case
              Selecting t ts -> Dragging t ts
              Dragging t ts -> Selecting t ts
        arr ->
          zoom (_Dragging . _1 . tilePos)
            $ case arr of
                KeyRight -> _1 += 1
                KeyLeft -> _1 -= 1
                KeyUp -> _2 += 1
                KeyDown -> _2 -= 1
                _ -> pure ()

ltrEvent :: Char -> St -> St
ltrEvent k =
  execState $ do
    zoom (_Dragging . _1)
      $ case k of
          'z' -> modify rotL
          'c' -> modify rotR
          'h' -> modify flipH
          'v' -> modify flipV
          _ -> pure ()

pollCoverage :: Handle -> IORef [CoverageInfo] -> IO ()
pollCoverage h sRef = do
  coverageLine <- hGetLine h
  case readMaybe coverageLine of
    Just (Stats updatedStats) -> atomicWriteIORef sRef updatedStats
    _ -> pure ()

pushCoverage :: (Show a) => Handle -> Chan a -> IO ()
pushCoverage h sChan = do
  coverageMsg <- readChan sChan
  hPrint h coverageMsg

startCommunication ::
     CmdArgs -> IORef [(Int, Int, Shard, Bool)] -> Chan InMsg -> IO ()
startCommunication (CmdArgs address port) sRef sChan = do
  sock <- socket AF_INET Stream 0
  addr <-
    addrAddress . head
      <$> getAddrInfo (Just defaultHints) (Just address) (Just port)
  connect sock addr
  h <- socketToHandle sock ReadWriteMode
  hSetBuffering h NoBuffering
  _ <- forkIO $ forever $ pushCoverage h sChan
  _ <- forkIO $ forever (pollCoverage h sRef)
  pure ()

allTiles :: Traversal' St Tile
allTiles f (Selecting t ts) = Selecting <$> f t <*> traverse f ts
allTiles f (Dragging t ts) = Dragging <$> f t <*> traverse f ts

notifyServer :: ClientState -> IO ()
notifyServer cs = do
  let st = cs ^. canvasState
      coverage =
        [ (tx + sx, ty + sy, shard)
        | Tile (tx, ty) subs <- toListOf allTiles st
        , ((sx, sy), shards) <- subs
        , shard <- shards
        ]
  writeChan (cs ^. pushChan) (Cover coverage)

cmdArgsParser :: Parser CmdArgs
cmdArgsParser =
  CmdArgs
    <$> strOption
          (long "address"
             <> short 'a'
             <> metavar "HOST"
             <> help "Address of server to connect to")
    <*> strOption
          (long "port"
             <> short 'p'
             <> help "Server port to connect to"
             <> showDefault
             <> value "8080"
             <> metavar "INT")

main :: IO ()
main = do
  let opts =
        info
          (cmdArgsParser <**> helper)
          (fullDesc
             <> progDesc "Simple client for creating and sharing logo designs")
  args <- execParser opts
  sChan <- newChan
  sRef <- newIORef []
  startCommunication args sRef sChan
  let initClient =
        case startingTiles of
          t:ts -> ClientState (Selecting t ts) sRef sChan
          [] -> error "startingTiles must not be empty"
  notifyServer initClient
  playIO FullScreen white 20 initClient render event (\_ cs -> pure cs)
