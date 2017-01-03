-- MPD bindings (barely) for Haskell.
-- Copyright (C) 2006 Evan Martin <martine@danga.com>

module MPD (
  Connection, connect,
  runCommand, runCommand_,
  setVolume, getVolume,

  ReconnectableConnection, rcConnect,
  rcConnection, rcDo
) where

import qualified Network
import System.IO
import System.IO.Error
import Data.List
-- import System.Posix

-- for ReconnectableConnection
import Data.IORef

type Connection = Handle

{-
ignoreSIGPIPE = installHandler sigPIPE Ignore Nothing
-}

connect :: (String, Int) -> IO Connection
connect (host, port) = do
  handle <- Network.connectTo host (Network.PortNumber $ fromIntegral port)
  hSetBuffering handle LineBuffering
  welcome <- hGetLine handle
  putStrLn welcome
  return handle

runCommand :: Handle -> String -> IO [String]
runCommand conn cmd = do
  hPutStrLn conn cmd
  lines <- getResponseLines []
  return $ reverse lines where
    getResponseLines :: [String] -> IO [String]
    getResponseLines ls = do
      line <- hGetLine conn
      if line == "OK"
        then return ls
        else getResponseLines (line:ls)

runCommand_ :: Handle -> String -> IO ()
runCommand_ conn cmd = runCommand conn cmd >> return ()

getVolume :: Connection -> IO Int
getVolume conn = do
  lines <- runCommand conn "status"
  let volume_line = lines !! 0  -- XXX hack; should actually parse this
  let volume_key = "volume: "
  if volume_key `isPrefixOf` volume_line
    then return $ read (drop (length volume_key) volume_line)
    else return (-1)

setVolume :: Connection -> Int -> IO ()
setVolume conn v = runCommand_ conn ("setvol " ++ show v)

withReconnect :: Connection -> (String, Int) -> (Connection -> IO a) -> IO (Connection, a)
withReconnect conn addr func = do
  do { ret <- func conn; return (conn, ret) } `catchIOError` \e -> do
         -- print $ ioeGetErrorString e
         newconn <- MPD.connect addr
         ret <- func newconn
         return (newconn, ret)

type ReconnectableConnection = (IORef Connection, (String, Int))
rcConnect :: (String, Int) -> IO ReconnectableConnection
rcConnect addr = do
  conn <- connect addr
  connref <- newIORef conn
  return (connref, addr)

rcConnection :: ReconnectableConnection -> IO Connection
rcConnection (connref, _) = readIORef connref

rcDo :: ReconnectableConnection -> (Connection -> IO a) -> IO a
rcDo (connref, addr) job = do
  conn <- readIORef connref
  (newconn, ret) <- withReconnect conn addr job
  writeIORef connref newconn
  return ret

-- vim: set ts=2 sw=2 et ft=haskell :
