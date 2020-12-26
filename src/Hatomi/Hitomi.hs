{-# LANGUAGE OverloadedStrings #-}

module Hatomi.Hitomi where

import Data.Char
import Data.String
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.Aeson

import Network.URI
import Network.HTTP.Client

import Control.Applicative

import Hatomi

galleryInfoUrl :: GalleryId -> String
galleryInfoUrl gid = "ltn.hitomi.la/galleries/" ++ show gid ++ ".js"

galleryBlockUrl :: GalleryId -> String
galleryBlockUrl gid = "ltn.hitomi.la/galleryblock/" ++ show gid ++ ".html"

galleryIntroUrl :: GalleryInfo -> String
galleryIntroUrl GalleryInfo{_id=gid, _title=gtitle, _type=gtype, _language_localname=glang} =
  escapeURIString p $
    "hitomi.la/" ++ T.unpack gtype ++ "/" ++ replace (T.unpack gtitle ++ "-" ++ T.unpack glang) ++ "-" ++ show gid ++ ".html"
  where
    p x = isAllowedInURI x || x == '|'
    replace [] = []
    replace (x:xs)
      | x == ' '              = '-' : replace xs
      | 'A' <= x && x <= 'Z'  = toLower x : replace xs
      | otherwise             = x : replace xs

galleryImageUrl :: ImageInfo -> String
galleryImageUrl ImageInfo{name=name, hash=hash}
  | [x,y,z] <- T.unpack . T.takeEnd 3 $ hash
    = sub x y:"" ++ "b.hitomi.la/images/" ++ z:'/':x:y:"/" ++ T.unpack hash ++ "." ++ ext
  | otherwise = ""
  where
    ext = T.unpack . last . T.splitOn "." $ name
    sub x y = if g < 0x09
      then chr (ord 'a' + 1 `mod` d)
      else chr (ord 'a' + g `mod` d)
      where
        hex c
          | '0' <= c && c <= '9' = ord c - ord '0'
          | 'a' <= c && c <= 'z' = ord c - ord 'a' + 16
          | otherwise            = 0
        g = hex x * 16 + hex y
        d = if g < 0x30 then 2 else 3

fetchGalleryInfo :: GalleryId -> Fetch a
fetchGalleryInfo gid man reader = do
  request <- parseRequest $ "https://" ++ galleryInfoUrl gid
  withResponse request (connectionManager man) reader

fetchGalleryBlock :: GalleryId -> Fetch a
fetchGalleryBlock gid man reader = do
  request <- parseRequest $ "https://" ++ galleryBlockUrl gid
  withResponse request (connectionManager man) reader

fetchGalleryImage :: GalleryInfo -> ImageInfo -> Fetch a
fetchGalleryImage ginfo iinfo man reader = do
  _request <- parseRequest $ "https://" ++ galleryImageUrl iinfo
  let request = _request  { requestHeaders = headers
                          , responseTimeout = responseTimeoutMicro 60000000
                          }
  withResponse request (connectionManager man) reader
  where
    headers = [("Referer", fromString $ "https://" ++ galleryIntroUrl ginfo)]

parseGalleryInfo :: BSL.ByteString -> Maybe GalleryInfo
parseGalleryInfo = decode' . BSL.tail . BSL.dropWhile (/=c)
  where
    c = fromIntegral $ ord '='

parseGalleryBlock :: BSL.ByteString -> Maybe GalleryBlock
parseGalleryBlock _ = Nothing

