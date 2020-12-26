{-# LANGUAGE OverloadedStrings #-}

module Hatomi where

import Data.Text as T
import Data.Aeson

import Control.Applicative

import Network.HTTP.Client

data HatomiManager = HatomiManager
  { hatomiDirectory   :: FilePath
  , connectionManager :: Manager
  }

type Fetch a = HatomiManager -> (Response BodyReader -> IO a) -> IO a

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

