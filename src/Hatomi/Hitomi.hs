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

data Config = Config
  { connectionManager :: Manager
  }

type Fetch a = Config -> (Response BodyReader -> IO a) -> IO a

type GalleryId = Int

type GalleryType = T.Text

data ImageInfo = ImageInfo
  { name    :: T.Text
  , hash    :: T.Text
  , width   :: Int
  , height  :: Int
  , haswebp :: Bool
  , hasavif :: Bool
  } deriving Show

data Tag = Male     T.Text
         | Female   T.Text
         | Unisex   T.Text
         deriving Show

data GalleryInfo = GalleryInfo
  { _id         :: GalleryId
  , _title      :: T.Text
  , _type       :: GalleryType
  , _files      :: [ImageInfo]
  , _language   :: T.Text
  , _language_localname :: T.Text
  , _tags       :: [Tag]
  , _date       :: T.Text
  } deriving Show

data GalleryBlock = GalleryBlock deriving Show

instance FromJSON ImageInfo where
  parseJSON = withObject "ImageInfo" $ \v ->
    ImageInfo
    <$> (v .: "name"    <|> pure "")
    <*> (v .: "hash"    <|> pure "")
    <*> (v .: "width"   <|> pure 0)
    <*> (v .: "height"  <|> pure 0)
    <*> (fmap (/=(0::Int)) (v .: "haswebp") <|> pure False)
    <*> (fmap (/=(0::Int)) (v .: "hasavif") <|> pure False)

instance FromJSON Tag where
  parseJSON = withObject "ImageInfo" $ \v ->
    (sex v <|> pure Unisex) <*> v .: "tag"
    where
      sex v = do
        male    <- v .: "male"
        female  <- v .: "female"
        if (male == String "1" || male == Number 1) && female == String "" then
          pure Male
        else if male == String "" && (female == String "1" || female == Number 1) then
          pure Female
        else
          pure Unisex

instance FromJSON GalleryInfo where
  parseJSON = withObject "GalleryInfo" $ \v ->
    GalleryInfo
    <$> ((read :: String -> GalleryId) <$> v .: "id")
    <*> (v .: "title"     <|> pure "")
    <*> (v .: "type"      <|> pure "")
    <*> (v .: "files"     <|> pure [])
    <*> (v .: "language"  <|> pure "")
    <*> (v .: "language_localname" <|> pure "")
    <*> (v .: "tags"      <|> pure [])
    <*> (v .: "date"      <|> pure "")

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
fetchGalleryInfo gid config reader = do
  request <- parseRequest $ "https://" ++ galleryInfoUrl gid
  withResponse request (connectionManager config) reader

fetchGalleryBlock :: GalleryId -> Fetch a
fetchGalleryBlock gid config reader = do
  request <- parseRequest $ "https://" ++ galleryBlockUrl gid
  withResponse request (connectionManager config) reader

fetchGalleryImage :: GalleryInfo -> ImageInfo -> Fetch a
fetchGalleryImage ginfo iinfo config reader = do
  _request <- parseRequest $ "https://" ++ galleryImageUrl iinfo
  let request = _request  { requestHeaders = headers
                          , responseTimeout = responseTimeoutMicro 60000000
                          }
  withResponse request (connectionManager config) reader
  where
    headers = [("Referer", fromString $ "https://" ++ galleryIntroUrl ginfo)]

parseGalleryInfo :: BSL.ByteString -> Maybe GalleryInfo
parseGalleryInfo = decode' . BSL.tail . BSL.dropWhile (/=c)
  where
    c = fromIntegral $ ord '='

parseGalleryBlock :: BSL.ByteString -> Maybe GalleryBlock
parseGalleryBlock _ = Nothing

