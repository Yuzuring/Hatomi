{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Void
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString.Lazy.Internal as BSL (ByteString(..))
import qualified Data.Text as T

import System.Environment
import System.Directory
import System.Clock

import qualified Network.HTTP.Client      as N
import qualified Network.HTTP.Client.TLS  as N

import Control.Monad.Cont
import Control.Monad.IO.Class

import Control.Concurrent.MVar
import Control.Concurrent.Async

import Hatomi.Hitomi

data Progress = Progress
  { total :: !Int     -- total size       (bytes)
  , done  :: !Int     -- downloaded size  (bytes)
  , rate  :: !Double  -- download speed   (bytes/sec)
  } deriving Show

readResponseBody :: MVar Progress -> N.Response N.BodyReader -> IO BSL.ByteString
readResponseBody mvar res = do
    t0 <- getTime Monotonic
    let p0 = Progress contentLength 0 0.0
    putMVar mvar p0
    go t0 p0 []
  where
    go :: TimeSpec -> Progress -> [BS.ByteString] -> IO BSL.ByteString
    go t p cs = do
      c <- N.brRead bodyReader
      t' <- getTime Monotonic
      let dt = (fromIntegral . toNanoSecs $ diffTimeSpec t t') / 1.0e9
          dx = BS.length c
          r' = fromIntegral dx / dt
          p' = p {done = done p + dx, rate = 0.6 * rate p + 0.4 * r'}
      if dx /= 0 then do
        tryTakeMVar mvar
        putMVar mvar p'
        go t' p' (c:cs)
      else
        return $ foldl' (\s c -> BSL.Chunk c s) BSL.Empty cs
      
    status      = N.responseStatus res
    headers     = N.responseHeaders res
    bodyReader  = N.responseBody res
    
    _contentType   = lookup "Content-Type" headers
    Just _contentLength = lookup "Content-Length" headers
    contentLength = read . BSC.unpack $ _contentLength

main :: IO ()
main = flip runContT pure . callCC $ \k ->
  let exit = absurd <$> k ()
  in do
  _home <- liftIO $ lookupEnv "HOME"
  homeDir <- case _home of
    Nothing -> do
      liftIO $ putStrLn "no $HOME"
      exit
    Just homeDir -> pure homeDir

  hasConfigFile <- liftIO $ doesFileExist (homeDir ++ "/.config/hatomirc")
  unless hasConfigFile $ do
    liftIO $ putStrLn "no ~/.config/hatomirc"
    exit

  args <- liftIO getArgs
  when (null args) $ do
    liftIO $ putStrLn "no args"
    exit
  
  liftIO $ do
    manager <- N.newManager N.tlsManagerSettings
    let config = Config manager
        gid = read (head args) :: GalleryId
    
    mvar <- newEmptyMVar
    aprint <- async (printer mvar)

    Just info <- parseGalleryInfo . N.responseBody <$> fetchGalleryInfo gid config    
    aimg <- async $ fetchGalleryImage info (_files info !! 0) (readResponseBody mvar) config
    img <- wait aimg
    cancel aprint    

    createDirectoryIfMissing True (homeDir ++ "/hatomi/" ++ show gid)
    BSL.writeFile (homeDir ++ "/hatomi/" ++ show gid ++ "/" ++ (T.unpack . name) (_files info !! 0)) img
    return ()
  where
    printer :: MVar Progress -> IO ()
    printer mvar = do
      progress <- takeMVar mvar
      print progress
      printer mvar
