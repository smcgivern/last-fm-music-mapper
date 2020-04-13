module MusicMapper
  ( module Lastfm
  , Artist
  , userArtists
  , artistCountries
  ) where

import Control.Lens
import Countries
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.Text
import GHC.Generics
import Lastfm (apiKey, artist, json, lastfm, limit, newConnection, period, user)
import Lastfm.Artist (getTopTags)
import Lastfm.User (getTopArtists)

import qualified Data.FileCache as FC
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Vector as V

data Artist = Artist
  { countries :: [Country]
  , imageUrl :: Text
  , name :: Text
  , playcount :: Int
  , url :: Text
  }
  deriving (Eq, Generic, Show, ToJSON)

data Image = Image
  { size :: Text
  , text :: Text
  }
  deriving (Eq, Generic, Show, ToJSON)

parseImages :: Value -> Parser (Text, Text)
parseImages =
  withObject "<image>" $ \obj -> do
    size <- obj .: "size"
    url <- obj .: "#text"
    return (size, url)

parseArtists :: Value -> Parser [Artist]
parseArtists = withObject "Artist" $ \obj -> do
  top <- obj .: "topartists"
  top .: "artist"


instance FromJSON Artist where
  parseJSON = withObject "Artist" $ \obj -> do
    imageUrl <- do
      image <- obj .: "image"
      images <- withArray "<images>" (\arr -> mapM parseImages (V.toList arr)) image
      return $ Maybe.fromMaybe ("", "") $ List.find (\x -> (fst x) == "small") images

    name <- obj .: "name"
    playcount <- obj .: "playcount"
    url <- obj .: "url"

    return $ Artist
      { countries = []
      , imageUrl = snd imageUrl
      , name = name
      , playcount = read playcount :: Int
      , url = url
      }

getUserTopArtists conn key username periodId = do
  response <- lastfm conn $ getTopArtists <*> user username <* limit 1000 <* period periodId <*> apiKey key <* Lastfm.json
  case response of
    Left _ -> return $ Left []
    Right x -> return $ Right x

userArtists cache conn key username periodId = do
  let cacheFile = unpack $ Data.Text.concat ["tmp/", username, "-", periodId, ".cache"]
  response <- FC.lazyQuery cache cacheFile $ getUserTopArtists conn key username periodId

  let artistsList response = do
        case response of
          Left _ -> []
          Right x -> Maybe.fromMaybe [] $ parseMaybe parseArtists x

  let updateCountries artist = do
        countries <- artistCountries cache conn key (name artist)
        return $ artist { countries = countries }

  updated <- mapM updateCountries $ artistsList response

  return updated

artistTags response = do
  case response of
    Left _ -> return $ []
    Right x -> return $ toListOf (key "toptags" . key "tag" . values . key "name" . _String) x

getArtistTopTags conn key artistName = do
  response <- lastfm conn $ getTopTags <*> artist artistName <*> apiKey key <* Lastfm.json
  case response of
    Left _ -> return $ Left []
    Right x -> return $ Right x

artistCountries cache conn key artistName = do
  let cacheFile = unpack $ Data.Text.concat ["tmp/", artistName, ".cache"]
  response <- FC.lazyQuery cache cacheFile $ getArtistTopTags conn key artistName
  tags <- artistTags response
  return $ countriesFor tags
