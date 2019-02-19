{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ImpredicativeTypes    #-}

module Database.Schema where

import           Database.Beam
import           Database.Beam.Backend
import           Database.Beam.Migrate
import           Database.Beam.Sqlite
import           Database.SQLite.Simple         ( open
                                                , Connection
                                                )

import           Data.Int                       ( Int64 )
import           Data.Text                      ( Text )
import           Data.Time                      ( UTCTime )
import           Data.UUID                      ( UUID )

import           Data.Aeson

-- Instructor

data InstructorT f = Instructor
  { _instructorInstructorId :: Columnar f Int64
  , _instructorFullName     :: Columnar f Text
  , _instructorFirstName    :: Columnar f (Maybe Text)
  , _instructorLastName     :: Columnar f (Maybe Text)
  , _instructorRating       :: Columnar f (Maybe Float)
  , _instructorUrl          :: Columnar f (Maybe Text)
  } deriving (Generic)

Instructor (LensFor instructorInstructorId) (LensFor instructorFullName) (LensFor instructorFirstName) (LensFor instructorLastName) (LensFor instructorRating) (LensFor instructorUrl)
  = tableLenses

instance ToJSON Instructor where
  toJSON (Instructor id fullName firstName lastName rating url) =
    object [ "id"        .= id
           , "fullName"  .= fullName
           , "firstName" .= firstName
           , "lastName"  .= lastName
           , "rating"    .= rating
           , "url"       .= url  ]

type Instructor = InstructorT Identity
type InstructorId = PrimaryKey InstructorT Identity

deriving instance Show Instructor
deriving instance Eq Instructor
deriving instance Show InstructorId
deriving instance Eq InstructorId

instance Beamable InstructorT
instance Beamable (PrimaryKey InstructorT)
instance Table InstructorT where
  data PrimaryKey InstructorT f = InstructorId (Columnar f Int64) deriving Generic
  primaryKey = InstructorId . _instructorInstructorId

-- Course

data CourseT f = Course
  { _courseId         :: Columnar f Int64
  , _courseName       :: Columnar f Text
  , _courseClass      :: Columnar f Text
  , _courseDiscipline :: Columnar f Text
  } deriving (Generic)

Course (LensFor courseId) (LensFor courseName) (LensFor courseClass) (LensFor courseDiscipline)
  = tableLenses

type Course = CourseT Identity
type CourseId = PrimaryKey CourseT Identity

deriving instance Show Course
deriving instance Eq Course
deriving instance Show CourseId
deriving instance Eq CourseId

instance Beamable CourseT
instance Beamable (PrimaryKey CourseT)
instance Table CourseT where
  data PrimaryKey CourseT f = CourseId (Columnar f Int64) deriving Generic
  primaryKey = CourseId . _courseId

instance ToJSON Course where
  toJSON (Course courseId name cclass discipline) =
    object [ "courseId"   .= courseId
           , "name"       .= name
           , "cclass"     .= cclass
           , "discipline" .= discipline ]

-- TODO: yank these ToJSON statements to Api.hs

-- Term

data TermT f = Term
  { _termDate         :: Columnar f Int64
  , _termDescription  :: Columnar f Text
  } deriving (Generic)

Term (LensFor termDate) (LensFor termDescription) = tableLenses

type Term = TermT Identity
type TermDate = PrimaryKey TermT Identity

deriving instance Show Term
deriving instance Eq Term
deriving instance Show TermDate
deriving instance Eq TermDate

instance Beamable TermT
instance Beamable (PrimaryKey TermT)
instance Table TermT where
  data PrimaryKey TermT f = TermDate (Columnar f Int64) deriving Generic
  primaryKey = TermDate . _termDate

instance ToJSON Term where
  toJSON (Term date description) =
    object [ "date"        .= date
           , "description" .= description ]

-- ClassOffering

data ClassOfferingT f = ClassOffering
  { _classOfferingId           :: Columnar f Int64
  , _classOfferingCourseId     :: PrimaryKey CourseT f
  , _classOfferingInstructorId :: PrimaryKey InstructorT f
  , _classOfferingTerm         :: PrimaryKey TermT f
  , _classOfferingCredits      :: Columnar f Int64
  , _classOfferingDays         :: Columnar f Text
  , _classOfferingTime         :: Columnar f Text
  , _classOfferingCrn          :: Columnar f Int64
  , _classOfferingTimestamp    :: Columnar f Text -- TODO: UTCTime
  } deriving (Generic)

ClassOffering (LensFor classOfferingId) (CourseId (LensFor classOfferingCourseId)) (InstructorId (LensFor classOfferingInstructorId)) (TermDate (LensFor classOfferingTerm)) (LensFor classOfferingCredits) (LensFor classOfferingDays) (LensFor classOfferingTime) (LensFor classOfferingCrn) (LensFor classOfferingTimestamp)
  = tableLenses

type ClassOffering = ClassOfferingT Identity
type ClassOfferingId = PrimaryKey ClassOfferingT Identity

deriving instance Show ClassOffering
deriving instance Eq ClassOffering
deriving instance Show ClassOfferingId
deriving instance Eq ClassOfferingId

instance Beamable ClassOfferingT
instance Beamable (PrimaryKey ClassOfferingT)
instance Table ClassOfferingT where
  data PrimaryKey ClassOfferingT f = ClassOfferingId (Columnar f Int64) deriving Generic
  primaryKey = ClassOfferingId . _classOfferingId

instance ToJSON ClassOffering where
  toJSON (ClassOffering classOfferingId courseId instructorId term credits days time crn timestamp) =
    object [ "classOfferingId" .= classOfferingId
           -- , "courseId"        .= courseId
           -- , "instructorId"    .= instructorId
           -- , "term"            .= term
           , "credits"         .= credits
           , "days"            .= days
           , "time"            .= time
           , "crn"             .= crn
           , "timestamp"       .= timestamp ]

-- Database

data ScheduleDB f = ScheduleDB
  { _scheduleCourse        :: f (TableEntity CourseT)
  , _scheduleInstructor    :: f (TableEntity InstructorT)
  , _scheduleTerm          :: f (TableEntity TermT)
  , _scheduleClassOffering :: f (TableEntity ClassOfferingT)
  } deriving (Generic)

instance Database be ScheduleDB

ScheduleDB (TableLens scheduleCourse) (TableLens scheduleInstructor) (TableLens scheduleTerm) (TableLens scheduleClassOffering)
  = dbLenses

scheduleDb :: DatabaseSettings be ScheduleDB
scheduleDb = defaultDbSettings `withDbModification` dbModification
  { _scheduleCourse        = modifyTable id $ tableModification
                               { _courseId         = fieldNamed "CourseId"
                               , _courseName       = fieldNamed "Name"
                               , _courseClass      = fieldNamed "Class"
                               , _courseDiscipline = fieldNamed "Discipline"
                               }
  , _scheduleInstructor    = modifyTable id $ tableModification
                               { _instructorInstructorId = fieldNamed
                                                             "InstructorId"
                               , _instructorFullName     = fieldNamed "FullName"
                               , _instructorFirstName = fieldNamed "FirstName"
                               , _instructorLastName     = fieldNamed "LastName"
                               , _instructorRating       = fieldNamed "Rating"
                               , _instructorUrl          = fieldNamed "URL"
                               }
  , _scheduleTerm          = modifyTable id $ tableModification
                               { _termDate        = fieldNamed "Date"
                               , _termDescription = fieldNamed "Description"
                               }
  , _scheduleClassOffering =
    modifyTable (\_ -> "ClassOffering") $ tableModification
      { _classOfferingId           = fieldNamed "ClassOfferingId"
      , _classOfferingCourseId     = CourseId $ fieldNamed "CourseId"
      , _classOfferingInstructorId = InstructorId "InstructorId"
      , _classOfferingTerm         = TermDate $ fieldNamed "Term"
      , _classOfferingCredits      = fieldNamed "Credits"
      , _classOfferingDays         = fieldNamed "Days"
      , _classOfferingTime         = fieldNamed "Time"
      , _classOfferingCrn          = fieldNamed "CRN"
      , _classOfferingTimestamp    = fieldNamed "Timestamp"
      }
  }


term1 :: Term
term1 = Term 1 "one"

instructor1 :: Instructor
instructor1 = Instructor 1
                         "Mark P. Jones"
                         (Just "Mark")
                         (Just "Jones")
                         (Just 4.8)
                         (Just "URL")

course1 :: Course
course1 = Course 1 "computer science" "cs100" "description"

classoffering1 :: ClassOffering
classoffering1 = ClassOffering 1
                               (CourseId 1)
                               (InstructorId 1)
                               (TermDate 1)
                               4
                               "TR"
                               "10:00-12:00"
                               12345
                               "12:00:00 PM"

terms1 :: [Term]
terms1 = [Term 1 "one", Term 2 "two"]


