{-# LANGUAGE OverloadedStrings #-}

module Query where

import           Control.Lens           (view, (^.), _1, _2, _3, _4)
import           Control.Monad.Reader   (ReaderT, ask, liftIO)
import           Data.Aeson             (ToJSON (..), object, (.=))
import           Data.Text.Internal
import           Database.Beam
import           Database.Beam.Query
import           Database.Beam.Sqlite
import           Database.Schema
import           Database.SQLite.Simple
import           GHC.Int
import           Servant                (Handler)

-- import qualified Model                  as Model

type ClassQueryResult =
  (ClassOffering
  , Course
  , Instructor
  , Term)

data ClassInstance = ClassInstance
  { classId    :: Int64
  , course     :: Course
  , instructor :: Maybe Instructor
  , term       :: Term
  , credits    :: Int64
  , days       :: Text
  , time       :: Text
  , crn        :: Int64
  , timestamp  :: Text -- UTCTime
  } deriving (Show)

instance ToJSON ClassInstance where
  toJSON (ClassInstance classId course instructor term credits days time crn timestamp) =
    object [ "id"         .= classId
           , "course"     .= course
           , "instructor" .= instructor
           , "term"       .= term
           , "credits"    .= credits
           , "days"       .= days
           , "time"       .= time
           , "crn"        .= crn
           , "timestamp"  .= timestamp ]


findClassList :: IO [ClassQueryResult]
findClassList = do
  conn <- open "app.db"
  runBeamSqlite conn $ runSelectReturningList $ select $ do
    classOffering <- all_     (scheduleDb ^. scheduleClassOffering)
    course        <- related_ (scheduleDb ^. scheduleCourse)
                              (_classOfferingCourseId classOffering)
    instructor    <- related_ (scheduleDb ^. scheduleInstructor)
                              (_classOfferingInstructorId classOffering)
    term          <- related_ (scheduleDb ^. scheduleTerm)
                              (_classOfferingTerm classOffering)
    pure (classOffering, course, instructor, term)


findClassInstances :: IO [ClassQueryResult]
findClassInstances = do
  conn <- open "app.db"
  runBeamSqlite conn $ runSelectReturningList $ select $ do
    classOffering <- all_     (scheduleDb ^. scheduleClassOffering)
    course        <- related_ (scheduleDb ^. scheduleCourse)
                              (_classOfferingCourseId classOffering)
    instructor    <- related_ (scheduleDb ^. scheduleInstructor)
                              (_classOfferingInstructorId classOffering)
    term          <- related_ (scheduleDb ^. scheduleTerm)
                              (_classOfferingTerm classOffering)
    --pure $ fmap toClassInstance (classOffering, course, instructor, term)
    pure (classOffering, course, instructor, term)

classList :: [ClassInstance]
classList = undefined
-- classList = do query <- findClassList
--                pure query


-- Stub elements for testing

course1 :: Course
course1 = Course 1 "functional programming" "cs457" "computer science"

instructor1 :: Instructor
instructor1 = Instructor 1 "mark p jones" (Just "mark") (Just "jones") (Just 4.8) (Just "url")

term1 :: Term
term1 = Term 20190101 "winter 2019"

classoffering1 :: ClassOffering
classoffering1 = ClassOffering
    1
    (CourseId     (_courseId course1))
    (InstructorId (_instructorInstructorId instructor1))
    (TermDate     (_termDate term1))
    4
    "TR"
    "08:00 am"
    40912
    "timestamp"

classQuery :: ClassQueryResult
classQuery = (classoffering1, course1, instructor1, term1)

toClassInstance :: ClassQueryResult -> ClassInstance
toClassInstance c = ClassInstance
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

getAllTerms :: ReaderT Connection Handler [Term]
getAllTerms = do
  conn <- ask
  liftIO
    $ runBeamSqliteDebug putStrLn conn
    $ runSelectReturningList
    $ select
    $ all_ (_scheduleTerm scheduleDb)

fullTermList :: IO [Term]
fullTermList = do
  conn <- open "app.db"
  runBeamSqliteDebug putStrLn conn $ runSelectReturningList $ select
    (all_ (_scheduleTerm scheduleDb))

fullClassList :: IO [ClassOffering]
fullClassList = do
  conn <- open "app.db"
  runBeamSqliteDebug putStrLn conn $ runSelectReturningList $ select
    (all_ (_scheduleClassOffering scheduleDb))
