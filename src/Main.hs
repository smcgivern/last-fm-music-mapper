{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import Data.Aeson
import GHC.Generics
import Network.Wai.Middleware.Static
import Text.Mustache
import Web.Scotty

import qualified Data.Text as Text
import qualified Data.Text.Lazy as Lazy
import qualified Text.Mustache.Compile.TH as TH

data Period = Period
  { identifier :: Text.Text
  , name :: Text.Text
  } deriving (Eq, Show, Generic)

instance ToJSON Period

data IndexPage = IndexPage
  { pageTitle :: String
  , periods :: [Period]
  } deriving (Eq, Show, Generic)

instance ToJSON IndexPage

defaultPeriods =
  [ Period { identifier = "7day", name = "Last 7 days" }
  , Period { identifier = "1month", name = "Last month" }
  , Period { identifier = "3month", name = "Last 3 months" }
  , Period { identifier = "6month", name = "Last 6 months" }
  , Period { identifier = "12month", name = "Last 12 months" }
  , Period { identifier = "overall", name = "Overall" }
  ]

main = scotty 3000 $ do
  middleware $ staticPolicy (addBase "./public")
  get "/" $ do
    username <- param "username" `rescue` (\_ -> next)
    period <- param "period" `rescue` (\_ -> next)

    redirect $ mconcat ["/:", username, "/", period, "/"]
  get "/" $ do
    let template = $(TH.compileMustacheFile "./template/index.html")
    let lowerPeriods = map (\x -> x { name = (Text.toLower (name x)) }) defaultPeriods
    let indexPage = IndexPage
                    { pageTitle = "Last.fm music mapper"
                    , periods = lowerPeriods
                    }

    html $ renderMustache template (toJSON indexPage)
  get (regex "^/:([^/]+)/?$") $ do
    username <- param "1"
    let period = Lazy.fromStrict $ identifier $ head defaultPeriods

    redirect $ mconcat ["/:", username, "/", period, "/"]
  get (regex "^/:([^/]+)/([^/]+)/?$") $ do
    username <- param "1"
    period <- param "2"

    html $ mconcat ["<h1>Hello, ", username, " (", period, ")!</h1>"]
