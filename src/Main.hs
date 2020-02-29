{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main (main) where

import Data.Aeson
import GHC.Generics
import Network.Wai.Middleware.Static
import Text.Mustache
import Web.Scotty

import qualified Text.Mustache.Compile.TH as TH

data IndexPage = IndexPage
  { pageTitle :: String
  } deriving (Eq, Show, Generic)

instance ToJSON IndexPage

main = scotty 3000 $ do
  middleware $ staticPolicy (addBase "./public")
  get "/" $ do
    username <- param "username" `rescue` (\_ -> next)
    redirect $ mconcat ["/:", username]
  get "/" $ do
    let template = $(TH.compileMustacheFile "./template/index.html")
    let indexPage = IndexPage { pageTitle = "Last.fm music mapper" }
    html $ renderMustache template (toJSON indexPage)
  get (regex "^/:([^/]+)/?$") $ do
    username <- param "1"
    html $ mconcat ["<h1>Hello, ", username, "!</h1>"]
