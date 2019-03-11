{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Exception.Base (evaluate)

import           Control.Lens           (view, (^.), _1, _2, _3, _4)
import           Data.Aeson             (ToJSON (..), object, (.=))
import           Data.Time              (LocalTime)
import           Data.Text.Internal     (Text)
import           Database.Schema
import           GHC.Int                (Int64)

type ClassQueryResult =
  (ClassOffering
  , Course
  , Instructor
  , Term)

data Class = Class
  { classId    :: Int64
  , course     :: Course
  , instructor :: Maybe Instructor
  , term       :: Term
  , credits    :: Int64
  , days       :: Text
  , time       :: Text
  , crn        :: Int64
  , timestamp  :: LocalTime
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
