{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Api where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.String.Conversions
-- import           Model
import           Database.Schema
import           Prelude.Compat
import           Query
import           Servant

type TermAPI = "terms" :> Get '[JSON] [Term]
type ClassOfferingAPI = "classes" :> Get '[JSON] [ClassOffering]
type CourseAPI = "courses" :> Get '[JSON] [Course]
type InstructorAPI = "instructors" :> Get '[JSON] [Instructor]
-- type NewAPI = "new" :> Get '[JSON] [Model.ClassOffering]
type ClassesAPI = "classes" :> Get '[JSON] [Class]

termAPI :: Proxy TermAPI
termAPI = Proxy :: Proxy TermAPI

classOfferingAPI :: Proxy ClassOfferingAPI
classOfferingAPI = Proxy :: Proxy ClassOfferingAPI

classesAPI :: Proxy ClassesAPI
classesAPI = Proxy :: Proxy ClassesAPI

-- newAPI :: Proxy NewAPI
-- newAPI = Proxy :: Proxy NewAPI

allAPI :: Proxy ClassQueryResult
allAPI = Proxy :: Proxy ClassQueryResult
