{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Model where

import           Data.Aeson   (ToJSON (..), object, (.=))
import           Data.Text    (Text)
import           GHC.Generics

data Instructor = Instructor
  { instructorId :: Int
  , fullName     :: Text
  , firstName    :: Maybe Text
  , lastName     :: Maybe Text
  , rating       :: Maybe Float
  , url          :: Maybe Text
  } deriving (Generic)

instance ToJSON Instructor where
  toJSON (Instructor id fullName firstName lastName rating url) =
    object [ "id"        .= id
           , "fullName"  .= fullName
           , "firstName" .= firstName
           , "lastName"  .= lastName
           , "rating"    .= rating
           , "url"       .= url  ]

data Course = Course
  { courseId   :: Int
  , name       :: Text
  , number     :: Text
  , discipline :: Text
  } deriving (Generic)

instance ToJSON Course where
  toJSON (Course courseId name number discipline) =
    object [ "id"         .= courseId
           , "name"       .= name
           , "number"     .= number
           , "discipline" .= discipline ]

data Term = Term
  { termDate        :: Int
  , termDescription :: Text
  } deriving (Generic)

instance ToJSON Term where
  toJSON (Term date description) =
    object [ "date"        .= date
           , "description" .= description ]

data ClassOffering = ClassOffering
  { classId    :: Int
  , course     :: Course
  , instructor :: Maybe Instructor
  , term       :: Term
  , credits    :: Int
  , days       :: Text
  , time       :: Text
  , crn        :: Int
  , timestamp  :: Text -- UTCTime
  } deriving (Generic)

instance ToJSON ClassOffering where
  toJSON (ClassOffering classId course instructor term credits days time crn timestamp) =
    object [ "id"         .= classId
           , "course"     .= course
           , "instructor" .= instructor
           , "term"       .= term
           , "credits"    .= credits
           , "days"       .= days
           , "time"       .= time
           , "crn"        .= crn
           , "timestamp"  .= timestamp ]

type ClassQueryResult =
  (ClassOffering
  , Course
  , Instructor
  , Term)

