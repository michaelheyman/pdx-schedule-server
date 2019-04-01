{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

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
