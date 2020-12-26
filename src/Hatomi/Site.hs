{-# LANGUAGE TypeFamilies #-}

module Hatomi.Site where

import Network.HTTP.Client

import qualified Hatomi as Hatomi
import Hatomi (GalleryId, HatomiManager)

import qualified Data.ByteString.Lazy as BSL

type Download a = (Request -> IO BSL.ByteString) -> IO a

class Site s where
  data GalleryInfo s :: *
  data ImageInfo s   :: *
  downloadGalleryInfo   :: GalleryId -> Download (GalleryInfo s)
  downloadGalleryImage  :: GalleryInfo s -> ImageInfo s -> Download BSL.ByteString
  imageInfos :: GalleryInfo s -> [ImageInfo s]
  toHatomiGalleryInfo :: GalleryInfo s -> Hatomi.GalleryInfo
  toHatomiImageInfo :: ImageInfo s -> Hatomi.ImageInfo
