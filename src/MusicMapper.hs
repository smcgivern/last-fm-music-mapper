{-# LANGUAGE OverloadedStrings #-}

module MusicMapper (module Lastfm, userArtists, artistCountries) where

import Control.Lens
import Data.Aeson.Lens
import Data.Text
import Lastfm

import Data.Aeson (Value)

import qualified Countries as Countries
import qualified Lastfm.Artist as Artist
import qualified Lastfm.User as User

artistNames response = do
  case response of
    Left _ -> []
    Right x -> toListOf (key "topartists" . key "artist" . values . key "name" . _String) x

userArtists conn key username periodId = do
  response <- lastfm conn $ User.getTopArtists <*> user username <* limit 1000 <* period periodId <*> apiKey key <* json
  return $ artistNames response

artistTags response = do
  case response of
    Left _ -> return $ []
    Right x -> return $ toListOf (key "toptags" . key "tag" . values . key "name" . _String) x

artistCountries conn key artistName = do
  response <- lastfm conn $ Artist.getTopTags <*> artist artistName <*> apiKey key <* json
  tags <- artistTags response
  return $ Countries.countriesFor tags
