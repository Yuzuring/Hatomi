{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}

module Main where

import Data.Void
import Data.List
import Data.List.Index
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy         as BSL
import qualified Data.ByteString.Lazy.Internal as BSL (ByteString(..))
import qualified Data.Text as T
import Data.Aeson

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
import Hatomi.Site
import Hatomi.Site.Hitomi (Hitomi)

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

progressBar :: MVar Progress -> IO ()
progressBar mp = do
  Progress t x r <- takeMVar mp
  Just (TS.Window h w) <- TS.size
  let n = x * (w-2) `div` t
  putStr "\r["
  putStr $ replicate n '#'
  putStr $ replicate (w-n-2) ' '
  putChar ']'
  if t == x then do
    putChar '\n'
    hFlush stdout
  else do
    hFlush stdout
    progressBar mp

downloadHitomiGallery :: GalleryId -> HatomiManager -> IO ()
downloadHitomiGallery gid man = do
  putStrLn ("Downloading " ++ show gid)
  let galleryDirectory = hatomiDirectory man ++ "/" ++ show gid
  createDirectoryIfMissing True galleryDirectory

  let manager = connectionManager man
  ginfo <- downloadGalleryInfo @Hitomi gid (\req -> responseBody <$> httpLbs req manager)
  let iinfos = imageInfos ginfo
      n = length iinfos
  iforM_ iinfos $ \i iinfo -> do
    putStrLn ("[" ++ show (i+1) ++ " of " ++ show n ++ "]")
    mp <- newEmptyMVar
    aimg <- async $ downloadGalleryImage ginfo iinfo (\req -> withResponse req manager $ readGalleryImage mp)
    progressBar mp
    img <- wait aimg
    BSL.writeFile (galleryDirectory ++ "/" ++ (T.unpack . Hatomi.name . toHatomiImageInfo) iinfo) img
  BSL.writeFile (galleryDirectory ++ "/meta.json") (encode $ toHatomiGalleryInfo ginfo)

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
    downloadHitomiGallery gid man 
