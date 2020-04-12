{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import Data.Aeson
import GHC.Generics
import Network.Wai.Middleware.Static
import Text.Mustache
import Web.Scotty

import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified MusicMapper as MusicMapper
import qualified Network.Wai as Wai
import qualified Text.Mustache.Compile.TH as TH

data Config = Config
  { key :: Text.Text
  , port :: Int
  , root :: Text.Text -- leading slash if non-empty; no trailing slash
  } deriving (Show, Generic)

instance FromJSON Config

data Period = Period
  { identifier :: Text.Text
  , name :: Text.Text
  } deriving (Eq, Generic, Show)

instance ToJSON Period

data IndexPage = IndexPage
  { indexBaseUrl :: Text.Text
  , periods :: [Period]
  } deriving (Eq, Generic, Show)

instance ToJSON IndexPage

data UserPage = UserPage
  { userBaseUrl :: Text.Text
  , username :: Text.Text
  , lowerPeriodName :: Text.Text
  , artists :: [MusicMapper.Artist]
  } deriving (Eq, Generic, Show)

instance ToJSON UserPage

defaultPeriods =
  [ Period { identifier = "7day", name = "Last 7 days" }
  , Period { identifier = "1month", name = "Last month" }
  , Period { identifier = "3month", name = "Last 3 months" }
  , Period { identifier = "6month", name = "Last 6 months" }
  , Period { identifier = "12month", name = "Last 12 months" }
  , Period { identifier = "overall", name = "Overall" }
  ]

setBaseUrl baseUrl app request respond = do
  let oldPathInfo = Wai.pathInfo request
  let newPathInfo = case List.stripPrefix (tail $ Text.splitOn "/" baseUrl) oldPathInfo of
        Just x -> x
        Nothing -> ["404-not-found"] -- otherwise this will match against the root, too
  app (request { Wai.pathInfo = newPathInfo }) respond

run config conn = do
  let baseUrl = root config

  middleware $ setBaseUrl baseUrl
  middleware $ staticPolicy (addBase "./public")

  get "/" $ do
    username <- param "username" `rescue` (\_ -> next)
    period <- param "period" `rescue` (\_ -> next)

    redirect $ mconcat [Lazy.fromStrict baseUrl, "/:", username, "/", period, "/"]

  get "/" $ do
    let template = $(TH.compileMustacheFile "./template/index.html")
    let lowerPeriods = map (\x -> x { name = (Text.toLower (name x)) }) defaultPeriods
    let indexPage = IndexPage
                    { indexBaseUrl = baseUrl
                    , periods = lowerPeriods
                    }

    html $ renderMustache template (toJSON indexPage)

  get (regex "^/:([^/]+)/?$") $ do
    username <- param "1"
    let period = Lazy.fromStrict $ identifier $ head defaultPeriods

    redirect $ mconcat [Lazy.fromStrict baseUrl, "/:", username, "/", period, "/"]

  get (regex "^/:([^/]+)/([^/]+)/?$") $ do
    username <- param "1"
    periodId <- param "2"

    let template = $(TH.compileMustacheFile "./template/user.html")
    let period = Maybe.fromMaybe (head defaultPeriods) (List.find (\x -> (identifier x) == periodId) defaultPeriods)

    artists <- liftAndCatchIO $ MusicMapper.userArtists conn (key config) username (identifier period)

    let userPage = UserPage
                   { userBaseUrl = baseUrl
                   , username = username
                   , lowerPeriodName = Text.toLower $ name $ period
                   , artists = artists
                   }

    html $ renderMustache template (toJSON userPage)

main = do
  config <- do
    parseResult <- decodeFileStrict' "config.json"
    case parseResult of
      Just c -> return c
      Nothing -> return Config { key = "", port = 3000, root = "" }

  conn <- MusicMapper.newConnection

  scotty (port config) (run config conn)
