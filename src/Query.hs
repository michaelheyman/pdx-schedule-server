module Query where

import           Control.Lens           (view, (^.))
import           Database.Beam
import           Database.Beam.Query
import           Database.Beam.Sqlite
import           Database.Schema
import           Database.SQLite.Simple

import qualified Model                  as Model

type ClassQueryResult =
  (ClassOffering
  , Course
  , Instructor
  , Term)

findClassList :: IO [ClassQueryResult]
findClassList = do
  conn <- open "app.db"
  runBeamSqlite conn $ runSelectReturningList $ select $ do
    classOffering <- all_ (scheduleDb ^. scheduleClassOffering)
    course        <- related_ (scheduleDb ^. scheduleCourse)
                              (_classOfferingCourseId classOffering)
    instructor <- related_ (scheduleDb ^. scheduleInstructor)
                           (_classOfferingInstructorId classOffering)
    term <- related_ (scheduleDb ^. scheduleTerm)
                     (_classOfferingTerm classOffering)
    pure (classOffering, course, instructor, term)


toClassList :: ClassQueryResult -> Model.ClassOffering
toClassList (co, c, i, t) = undefined
