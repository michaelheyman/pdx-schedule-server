{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Lens           (view, _1, _2, _3, _4)
import           Data.Aeson             (ToJSON (..), object, (.=))
import           Data.Text.Internal     (Text)
import           Data.Time              (LocalTime)
import           Database.Schema
import           GHC.Int                (Int64)

type ClassQueryResult =
  (ClassOffering
  , Course
  , Instructor
  , Term)

data Class = Class
  { classClassId    :: Int64
  , classCourse     :: Course
  , classInstructor :: Maybe Instructor
  , classTerm       :: Term
  , classCredits    :: Int64
  , classDays       :: Text
  , classTime       :: Text
  , classCrn        :: Int64
  , classTimestamp  :: LocalTime
  } deriving (Show)

instance ToJSON Class where
  toJSON (Class classId course instructor term credits days time crn timestamp) =
    object [ "id"         .= classId
           , "course"     .= course
           , "instructor" .= instructor
           , "term"       .= term
           , "credits"    .= credits
           , "days"       .= days
           , "time"       .= time
           , "crn"        .= crn
           , "timestamp"  .= timestamp ]

toClass :: ClassQueryResult -> Class
toClass c = Class
  (_classOfferingId classOffering)
  course
  (Just instructor)
  term
  (_classOfferingCredits classOffering)
  (_classOfferingDays classOffering)
  (_classOfferingTime classOffering)
  (_classOfferingCrn classOffering)
  (_classOfferingTimestamp classOffering)
 where
  classOffering = view _1 c
  course        = view _2 c
  instructor    = view _3 c
  term          = view _4 c
