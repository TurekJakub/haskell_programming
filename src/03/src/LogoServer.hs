{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import qualified Control.Exception as E
import Control.Monad
import Data.Foldable
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Set.Internal as SI
import LogoProtocol
import Network.Socket
import System.IO
import Text.Read

type State = S.Set (Int, (Int, Int, Shard))

data ServerCom = ServerCom
  { inChan :: Chan (Int, InMsg)
  , outChan :: TChan OutMsg
  , clientIdCtr :: MVar Int
  }

newServerCom :: IO ServerCom
newServerCom = ServerCom <$> newChan <*> newBroadcastTChanIO <*> newMVar 0

splitAround :: Ord p => p -> SI.Set (p, b) -> (SI.Set (p, b), SI.Set (p, b))
splitAround i s =
  let (l, (_, r)) =
        S.spanAntitone ((<= i) . fst) <$> S.spanAntitone ((< i) . fst) s
   in (l, r)

makeStats ::
     (Foldable t, Ord a1, Ord b, Ord c)
  => t (a2, (a1, b, c))
  -> [(a1, b, c, Bool)]
makeStats st =
  map (\((x, y, s), n) -> (x, y, s, n >= threshold)) $ M.assocs counts
  where
    counts = foldl' cnt M.empty . map snd $ toList st
    cnt m x = M.alter ((<|> Just 0) . fmap succ) x m
    threshold :: Int
    threshold = maximum counts `div` 2

workerThread :: ServerCom -> State -> IO ()
workerThread com state = do
  let broadcast x = atomically $ writeTChan (outChan com) x
      continue s = broadcast (Stats $ makeStats s) >> workerThread com s
  msg <- readChan (inChan com)
  case msg of
    (_, Poll) -> continue state
    (i, Cover cs) ->
      let (l, r) = splitAround i state
       in continue $ l `SI.merge` S.fromList (map (i, ) cs) `SI.merge` r
    (i, Quit) ->
      let (l, r) = splitAround i state
       in continue $ l `SI.merge` r

main :: IO a
main =
  withSocketsDo $ do
    com <- newServerCom
    _ <- forkIO $ workerThread com S.empty
    E.bracket open close $ mainLoop com
  where
    open = do
      sock <- socket AF_INET Stream 0
      setSocketOption sock ReuseAddr 1
      bind sock $ SockAddrInet 10042 0
      listen sock 10
      return sock

mainLoop :: ServerCom -> Socket -> IO b
mainLoop com sock =
  forever $ do
    (c, _) <- accept sock
    forkIO $ E.bracket (setupConn c) hClose $ runConn com

setupConn :: Socket -> IO Handle
setupConn c = do
  h <- socketToHandle c ReadWriteMode
  hSetBuffering h NoBuffering
  return h

runConn :: ServerCom -> Handle -> IO ()
runConn com h = do
  clientId <- takeMVar $ clientIdCtr com
  clientIdCtr com `putMVar` succ clientId
  myChan <- atomically $ dupTChan (outChan com)
  sender <-
    forkIO . forever $ do
      msg <- atomically (readTChan myChan)
      putStrLn $ "--> (" ++ show clientId ++ ") " ++ show msg
      hPrint h msg
  let loop = do
        -- the filter here removes the \r (and other ugly stuff) typically sent
        -- by telnet and other manual neworkish tools
        cmd <- filter (>= ' ') <$> hGetLine h
        putStrLn $ "<-- (" ++ show clientId ++ ") " ++ show cmd
        case readMaybe cmd of
          Just Quit -> pure ()
          Just x -> do
            writeChan (inChan com) (clientId, x)
            loop
          Nothing
            | null cmd -> loop
          _ -> do
            hPrint h Error
            loop
  E.finally loop $ do
    killThread sender
    writeChan (inChan com) (clientId, Quit)
