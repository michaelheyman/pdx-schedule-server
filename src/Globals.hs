{-# LANGUAGE OverloadedStrings #-}

module Globals where

import           Data.Text (Text)

database :: String
database = "dist/resources/app.db"

bootstrapUrl :: Text
bootstrapUrl =
  "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0/css/bootstrap.min.css"

fontAwesomeUrl :: Text
fontAwesomeUrl =
  "https://maxcdn.bootstrapcdn.com/font-awesome/4.7.0/css/font-awesome.min.css"
