{-# LANGUAGE OverloadedStrings #-}

module Query where

import           Control.Lens           ((^.))
import           Database.Beam.Query    (all_, related_, runSelectReturningList,
                                         select)
import           Database.Beam.Sqlite   (runBeamSqliteDebug)
import           Database.Schema
import           Database.SQLite.Simple (open)
import           Globals                (database)
import           Types                  (Class, ClassQueryResult, toClass)

findClassList :: IO [ClassQueryResult]
findClassList = do
  conn <- open database
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
