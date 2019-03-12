{-# LANGUAGE OverloadedStrings #-}

module Query where

import           Control.Lens           ((^.))
import           Control.Monad.Reader   (ReaderT, ask, liftIO)
import           Data.Time              (Day (..), LocalTime (..),
                                         TimeOfDay (..))
import           Database.Beam.Query    (all_, guard_, related_,
                                         runSelectReturningList, select, val_,
                                         (==.))
import           Database.Beam.Sqlite   (runBeamSqlite, runBeamSqliteDebug)
import           Database.Schema
import           Database.SQLite.Simple (Connection, open)
import           Servant                (Handler)
import           Types                  (ClassQueryResult, Class, toClass)

findClassList :: IO [ClassQueryResult]
findClassList = do
  conn <- open "app.db"
  runBeamSqliteDebug putStrLn conn
    $ runSelectReturningList
    $ select
    $ do classOffering <- all_     (scheduleDb ^. scheduleClassOffering)
         course        <- related_ (scheduleDb ^. scheduleCourse)
                                   (_classOfferingCourseId classOffering)
         instructor    <- related_ (scheduleDb ^. scheduleInstructor)
                                   (_classOfferingInstructorId classOffering)
         term          <- related_ (scheduleDb ^. scheduleTerm)
                                   (_classOfferingTerm classOffering)
         return (classOffering, course, instructor, term)

getClasses :: IO [Class]
getClasses = findClassList >>= return . map toClass

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
  runBeamSqliteDebug putStrLn conn
    $ runSelectReturningList
    $ select (all_ (_scheduleTerm scheduleDb))

fullClassList :: IO [ClassOffering]
fullClassList = do
  conn <- open "app.db"
  runBeamSqliteDebug putStrLn conn
    $ runSelectReturningList
    $ select (all_ (_scheduleClassOffering scheduleDb))

findInstructors :: Connection -> IO ()
findInstructors conn = runBeamSqlite conn $ do
  instructors <- runSelectReturningList
    $ select (all_ (_scheduleInstructor scheduleDb))
  mapM_ (liftIO . print) instructors

-- listTerms :: ( IsSql92Syntax cmd, Sql92SanityCheck cmd
--              , MonadBeam cmd be hdl m)
--             => m [Term]
-- listTerms = runSelectReturningList $ select (all_ (_scheduleTerm scheduleDb))

findTerms :: Connection -> IO ()
findTerms conn = runBeamSqlite conn $ do
  terms <- runSelectReturningList $ select (all_ (_scheduleTerm scheduleDb))
  mapM_ (liftIO . print) terms

findMarkJones :: Connection -> IO ()
findMarkJones conn = runBeamSqlite conn $ do
  json <- runSelectReturningList $ select $ do
    instructors <- all_ (scheduleDb ^. scheduleInstructor)
    guard_ (instructors ^. instructorFullName ==. val_ "Mark P. Jones")
    return instructors
  mapM_ (liftIO . print) json

findTermList :: Connection -> IO [Term]
findTermList conn = runBeamSqlite conn $ runSelectReturningList $ select
  (all_ (_scheduleTerm scheduleDb))

ft :: IO [Term]
ft = do
  conn <- open "app.db"
  runBeamSqlite conn $ runSelectReturningList $ select
    (all_ (_scheduleTerm scheduleDb))

-- Stub elements for testing

timestamp1 :: LocalTime
timestamp1 = LocalTime { localDay = ModifiedJulianDay 0, localTimeOfDay = TimeOfDay { todHour = 22, todMin = 22, todSec = 22} }

course1 :: Course
course1 = Course 1 "functional programming" "cs457" "computer science"

instructor1 :: Instructor
instructor1 = Instructor 1 "mark p jones" (Just "mark") (Just "jones") (Just 4.8) (Just "url") timestamp1

term1 :: Term
term1 = Term 20190101 "winter 2019"

classOfferingList :: [ClassOffering]
classOfferingList = [ ClassOffering
    1
    (CourseId     (_courseCourseId course1))
    (InstructorId (_instructorInstructorId instructor1))
    (TermDate     (_termDate term1))
    4
    "TR"
    "08:00 am"
    40912
    timestamp1 ]

classoffering1 :: ClassOffering
classoffering1 = ClassOffering
    1
    (CourseId     (_courseCourseId course1))
    (InstructorId (_instructorInstructorId instructor1))
    (TermDate     (_termDate term1))
    4
    "TR"
    "08:00 am"
    40912
    timestamp1

classQuery :: ClassQueryResult
classQuery = (classoffering1, course1, instructor1, term1)

classList :: [Class]
classList = map toClass [classQuery]
