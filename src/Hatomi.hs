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

