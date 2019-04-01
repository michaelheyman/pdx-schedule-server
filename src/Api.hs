{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}
-- |
-- Module:      Api
-- Copyright:   (c) 2019 Michael Heyman
-- License:     MIT
-- Maintainer:  Michael Heyman <contact@mheyman.com>
-- Stability:   experimental
-- Portability: portable
--
-- Definition of the API endpoints.

module Api where

import           Lucid
import           Servant
import           Servant.HTML.Lucid (HTML)
import           Types

type API = "classes" :> Get '[JSON] [Class]
      :<|> Get '[HTML] (Html ())
      :<|> "static" :> Raw

api :: Proxy API
api = Proxy
