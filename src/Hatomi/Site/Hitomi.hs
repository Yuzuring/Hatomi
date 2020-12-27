{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Hatomi.Site.Hitomi where

import Data.Char
import Data.String
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Data.HashMap.Strict hiding (map)
import Data.Aeson

import Network.URI
import Network.HTTP.Client

import Control.Applicative

import Text.HTML.Parser
import Text.HTML.Tree

import qualified Hatomi as Hatomi
import Hatomi (GalleryId)
import Hatomi.Site

type GalleryType = T.Text

newtype Tag = Tag {toHatomiTag :: Hatomi.Tag} deriving Show

data GalleryBlock = GalleryBlock deriving Show

instance FromJSON (ImageInfo Hitomi) where
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
    Tag <$> ((sex v <|> pure Hatomi.Unisex) <*> v .: "tag")
    where
      sex v = do
        male    <- v .: "male"
        female  <- v .: "female"
        if (male == String "1" || male == Number 1) && female == String "" then
          pure Hatomi.Male
        else if male == String "" && (female == String "1" || female == Number 1) then
          pure Hatomi.Female
        else
          pure Hatomi.Unisex

instance FromJSON (GalleryInfo Hitomi) where
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

data Hitomi

instance Site Hitomi where
  data GalleryInfo Hitomi = GalleryInfo
    { _id         :: GalleryId
    , _title      :: T.Text
    , _type       :: GalleryType
    , _files      :: [ImageInfo Hitomi]
    , _language   :: T.Text
    , _language_localname :: T.Text
    , _tags       :: [Tag]
    , _date       :: T.Text
    } deriving Show

  data ImageInfo Hitomi = ImageInfo
    { name    :: T.Text
    , hash    :: T.Text
    , width   :: Int
    , height  :: Int
    , haswebp :: Bool
    , hasavif :: Bool
    } deriving Show

  downloadGalleryInfo gid fetch = do
    req <- parseRequest $ "https://" ++ galleryInfoUrl gid
    res <- fetch req
    let Just x = parseGalleryInfo res
    pure x

  downloadGalleryImage ginfo iinfo fetch = do
      _req <- parseRequest $ "https://" ++ galleryImageUrl iinfo
      let req = _req { requestHeaders = headers
                     , responseTimeout = responseTimeoutMicro 60000000
                     }
      fetch req
    where
      headers = [("Referer", fromString $ "https://" ++ galleryIntroUrl ginfo)]

  imageInfos GalleryInfo{_files=fs} = fs
  toHatomiGalleryInfo x =
    Hatomi.GalleryInfo
    { Hatomi._id          = _id x
    , Hatomi._title       = _title x
    , Hatomi._group       = ""
    , Hatomi._type        = findWithDefault Hatomi.Misc (_type x) Hatomi.galleryTypeMap
    , Hatomi._language    = _language x
    , Hatomi._series      = ""
    , Hatomi._characters  = []
    , Hatomi._tags        = map toHatomiTag (_tags x)
    , Hatomi._files       = map toHatomiImageInfo (_files x)
    }
  toHatomiImageInfo ImageInfo{name=name, hash=hash} = Hatomi.ImageInfo name hash

galleryInfoUrl :: GalleryId -> String
galleryInfoUrl gid = "ltn.hitomi.la/galleries/" ++ show gid ++ ".js"

galleryBlockUrl :: GalleryId -> String
galleryBlockUrl gid = "ltn.hitomi.la/galleryblock/" ++ show gid ++ ".html"

galleryIntroUrl :: GalleryInfo Hitomi -> String
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

galleryImageUrl :: ImageInfo Hitomi -> String
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

parseGalleryInfo :: BSL.ByteString -> Maybe (GalleryInfo Hitomi)
parseGalleryInfo = decode' . BSL.tail . BSL.dropWhile (/=c)
  where
    c = fromIntegral $ ord '='

