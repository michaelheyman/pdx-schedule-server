{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import Data.Text
import Data.Time (UTCTime)
import Servant.API
import Schema

type ClassOfferingAPI = "classes" :> Get '[JSON] [ClassOffering]
type TermAPI = "term" :> Get '[JSON] [Term]
