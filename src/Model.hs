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

term1 :: Term
term1 = Term 1 "Winter 2019"

instructor1 :: Instructor
instructor1 = Instructor 1
                         "Mark P. Jones"
                         (Just "Mark")
                         (Just "Jones")
                         (Just 4.8)
                         (Just "URL")

course1 :: Course
course1 = Course 1 "computer science" "cs100" "description"

classoffering1 :: [ClassOffering]
classoffering1 =
  [ ClassOffering 1
                  course1
                  (Just instructor1)
                  term1
                  4
                  "TR"
                  "10:00-12:00"
                  12345
                  "12:00:00 PM"
  ]

classoffering2 :: [ClassOffering]
classoffering2 =
  [ ClassOffering 1
                  course1
                  (Just instructor1)
                  term1
                  4
                  "TR"
                  "10:00-12:00"
                  12345
                  "12:00:00 PM"
  ]

type ClassQueryResult =
  (ClassOffering
  , Course
  , Instructor
  , Term)

