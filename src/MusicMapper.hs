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

import qualified Data.Cache as C
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
    s <- obj .: "size"
    u <- obj .: "#text"
    return (s, u)

parseArtists :: Value -> Parser [Artist]
parseArtists = withObject "Artist" $ \obj -> do
  top <- obj .: "topartists"
  top .: "artist"


instance FromJSON Artist where
  parseJSON = withObject "Artist" $ \obj -> do
    imgUrl <- do
      image <- obj .: "image"
      images <- withArray "<images>" (\arr -> mapM parseImages (V.toList arr)) image
      return $ Maybe.fromMaybe ("", "") $ List.find (\x -> (fst x) == "small") images

    artistName <- obj .: "name"
    artistPlaycount <- obj .: "playcount"
    artistUrl <- obj .: "url"

    return $ Artist
      { countries = []
      , imageUrl = snd imgUrl
      , name = artistName
      , playcount = read artistPlaycount :: Int
      , url = artistUrl
      }

getUserTopArtists conn k (username, periodId) = do
  response <- lastfm conn $ getTopArtists <*> user username <* limit 1000 <* period periodId <*> apiKey k <* Lastfm.json
  case response of
    Left _ -> return $ Left []
    Right x -> return $ Right x

userArtists cache conn k username periodId = do
  let cacheKey = (username, periodId)
  response <- C.fetchWithCache cache cacheKey $ getUserTopArtists conn k

  let artistsList r = do
        case r of
          Left _ -> []
          Right x -> Maybe.fromMaybe [] $ parseMaybe parseArtists x

  let updateCountries a = do
        c <- artistCountries cache conn k (name a)
        return $ a { countries = c }

  updated <- mapM updateCountries $ artistsList response

  return updated

artistTags response = do
  case response of
    Left _ -> return $ []
    Right x -> return $ toListOf (key "toptags" . key "tag" . values . key "name" . _String) x

getArtistTopTags conn k (artistName, _) = do
  response <- lastfm conn $ getTopTags <*> artist artistName <*> apiKey k <* Lastfm.json
  case response of
    Left _ -> return $ Left []
    Right x -> return $ Right x

artistCountries cache conn k artistName = do
  response <- C.fetchWithCache cache (artistName, "tags") $ getArtistTopTags conn k
  tags <- artistTags response
  return $ countriesFor tags
