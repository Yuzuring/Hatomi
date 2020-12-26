{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module Hatomi where

import Data.Text as T
import Data.Aeson

import Control.Applicative

import Network.HTTP.Client

import Data.HashMap.Strict

data HatomiManager = HatomiManager
  { hatomiDirectory   :: FilePath
  , connectionManager :: Manager
  }

type Fetch a = HatomiManager -> (Response BodyReader -> IO a) -> IO a

type GalleryId = Int

data GalleryType = Doujinshi
                 | Manga
                 | ArtistCG
                 | GameCG
                 | Western
                 | NonH
                 | ImageSet
                 | Cosplay
                 | AsianPorn
                 | Misc
                 deriving Show

data ImageInfo = ImageInfo
  { name    :: T.Text
  , hash    :: T.Text
  } deriving Show

data Tag = Male     T.Text
         | Female   T.Text
         | Unisex   T.Text
         deriving Show

data GalleryInfo = GalleryInfo
  { _id         :: GalleryId
  , _title      :: T.Text
  , _group      :: T.Text
  , _type       :: GalleryType
  , _language   :: T.Text
  , _series     :: T.Text
  , _characters :: [T.Text]
  , _tags       :: [Tag]
  , _files      :: [ImageInfo]
  } deriving Show

instance FromJSON GalleryType where
  parseJSON = withText "GalleryType" $ pure . flip (findWithDefault Misc) mapping
    where
      mapping = fromList
        [ ("doujinshi"  , Doujinshi )
        , ("manga"      , Manga     )
        , ("artistcg"   , ArtistCG  )
        , ("gamecg"     , GameCG    )
        , ("western"    , Western   )
        , ("non-h"      , NonH      )
        , ("imageset"   , ImageSet  )
        , ("cosplay"    , Cosplay   )
        , ("asianporn"  , AsianPorn )
        , ("misc"       , Misc      )
        ]

instance ToJSON GalleryType where
  toJSON = \case
    Doujinshi -> "doujinshi"
    Manga     -> "manga"
    ArtistCG  -> "artistcg"
    GameCG    -> "gamecg"
    Western   -> "western"
    NonH      -> "non-h"
    ImageSet  -> "imageset"
    Cosplay   -> "cosplay"
    AsianPorn -> "asianporn"
    Misc      -> "misc"

instance FromJSON ImageInfo where
  parseJSON = withObject "ImageInfo" $ \v ->
    ImageInfo
    <$> v .: "name"
    <*> v .: "hash"

instance ToJSON ImageInfo where
  toJSON x = object ["name" .= name x, "hash" .= hash x]
 
instance FromJSON Tag where
  parseJSON = withObject "Tag" $ \v ->
    sex v <*> v .: "tag"
    where
      sex v = fmap (mapping !) (v .: "sex")

      mapping :: HashMap T.Text (T.Text -> Tag)
      mapping = fromList
        [ ("male"   , Male    )
        , ("female" , Female  )
        , ("unisex" , Unisex  )
        ]

instance ToJSON Tag where
  toJSON = object . \case
    Male    tag -> ["sex" .= ("male"   :: T.Text) , "tag" .= tag]
    Female  tag -> ["sex" .= ("female" :: T.Text) , "tag" .= tag]
    Unisex  tag -> ["sex" .= ("unisex" :: T.Text) , "tag" .= tag]

instance FromJSON GalleryInfo where
  parseJSON = withObject "GalleryInfo" $ \v ->
    GalleryInfo
    <$> v .: "id"
    <*> v .: "title"
    <*> v .: "group"
    <*> v .: "type"
    <*> v .: "language"
    <*> v .: "series"
    <*> v .: "characters"
    <*> v .: "tags"
    <*> v .: "files"

instance ToJSON GalleryInfo where
  toJSON x = object
    [ "id"          .= _id          x
    , "title"       .= _title       x
    , "group"       .= _group       x
    , "type"        .= _type        x
    , "language"    .= _language    x
    , "series"      .= _series      x
    , "characters"  .= _characters  x
    , "tags"        .= _tags        x
    , "files"       .= _files       x
    ]
