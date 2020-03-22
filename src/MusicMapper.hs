{-# LANGUAGE OverloadedStrings #-}

module MusicMapper (userArtists) where

import Lastfm

import qualified Countries as Countries
import qualified Lastfm.Artist as Artist
import qualified Lastfm.User as User

userArtists conn key username periodId =
  lastfm conn $ User.getTopArtists <*> user username <* limit 1000 <* period periodId <*> apiKey key <* json

artistCountries conn key artistName =
  lastfm conn $ Artist.getTags  <*> artist artistName <*> apiKey key <* json
