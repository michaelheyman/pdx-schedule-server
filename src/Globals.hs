{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module:      Globals
-- Copyright:   (c) 2019 Michael Heyman
-- License:     MIT
-- Maintainer:  Michael Heyman <contact@mheyman.com>
-- Stability:   experimental
-- Portability: portable
--
-- Global strings that are used in the project.

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
