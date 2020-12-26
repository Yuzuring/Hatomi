{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Void
import Data.List
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString.Lazy.Internal as BSL (ByteString(..))
import qualified Data.Text as T

import System.IO
import System.Environment
import System.Directory
import System.Clock
import qualified System.Console.Terminal.Size as TS

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Control.Monad.Cont
import Control.Monad.IO.Class

import Control.Concurrent.MVar
import Control.Concurrent.Async

import Hatomi
import Hatomi.Hitomi

data Progress = Progress
  { total :: !Int     -- total size       (bytes)
  , done  :: !Int     -- downloaded size  (bytes)
  , rate  :: !Double  -- download speed   (bytes/sec)
  } deriving Show

readGalleryImage :: MVar Progress -> Response BodyReader -> IO BSL.ByteString
readGalleryImage mvar res = do
    t0 <- getTime Monotonic
    let p0 = Progress contentLength 0 0.0
    putMVar mvar p0
    go t0 p0 []
  where
    go :: TimeSpec -> Progress -> [BS.ByteString] -> IO BSL.ByteString
    go t p cs = do
      c <- brRead bodyReader
      t' <- getTime Monotonic
      let dt = (fromIntegral . toNanoSecs $ diffTimeSpec t t') / 1.0e9
          dx = BS.length c
          r' = fromIntegral dx / dt
          p' = p {done = done p + dx, rate = 0.8 * rate p + 0.2 * r'}
      if dx /= 0 then do
        tryTakeMVar mvar
        putMVar mvar p'
        go t' p' (c:cs)
      else
        return $ foldl' (\s c -> BSL.Chunk c s) BSL.Empty cs
      
    status      = responseStatus res
    headers     = responseHeaders res
    bodyReader  = responseBody res
    
    _contentType   = lookup "Content-Type" headers
    Just _contentLength = lookup "Content-Length" headers
    contentLength = read . BSC.unpack $ _contentLength

readGalleryInfo :: Response BodyReader -> IO GalleryInfo
readGalleryInfo res = do
  bss <- brConsume (responseBody res)
  let Just info = parseGalleryInfo . BSL.fromChunks $ bss
  return info

downloadGallery :: GalleryId -> HatomiManager -> IO ()
downloadGallery gid man = do
  let galleryDirectory = hatomiDirectory man ++ "/" ++ show gid
  createDirectoryIfMissing True galleryDirectory
  info <- fetchGalleryInfo gid man readGalleryInfo
  forM_ (_files info) $ \imginfo -> do
    mp <- newEmptyMVar
    aprogressBar <- async (progressBar mp)
    img <- fetchGalleryImage info imginfo man (readGalleryImage mp)
    cancel aprogressBar
    putChar '\n'
    BSL.writeFile (galleryDirectory ++ "/" ++ (T.unpack . name) imginfo) img
  where
    progressBar :: MVar Progress -> IO ()
    progressBar mp = do
      Progress t x r <- takeMVar mp
      Just (TS.Window h w) <- TS.size
      let n = x * (w-2) `div` t
      putStr "\r["
      putStr $ replicate n '#'
      putStr $ replicate (w-n-2) ' '
      putChar ']'
      hFlush stdout
      progressBar mp

main :: IO ()
main = flip runContT pure . callCC $ \k ->
  let exit = absurd <$> k ()
  in do
  homeDir <- liftIO $ getHomeDirectory

  xdgConfigDir <- liftIO $ getXdgDirectory XdgConfig ""
  hasConfigFile <- liftIO $ doesFileExist (xdgConfigDir ++ "/hatomirc")
  unless hasConfigFile $ do
    liftIO $ putStrLn "no config file"
    exit

  args <- liftIO getArgs
  when (null args) $ do
    liftIO $ putStrLn "no args"
    exit
  
  liftIO $ do
    manager <- newManager tlsManagerSettings
    let man = HatomiManager (homeDir ++ "/hatomi") manager
        gid = read (args !! 0) 
    downloadGallery gid man 
