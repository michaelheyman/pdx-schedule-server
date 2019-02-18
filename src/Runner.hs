{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ImpredicativeTypes    #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Runner where

import           Control.Lens                   ( (^.) )
import           Database.SQLite.Simple
import           Database.Beam
import           Database.Beam.Sqlite
-- import Database.Beam.Backend.SQL.SQL92 (IsSql92Syntax, Sql92SanityCheck)
import           Api
import           Server

import           Schema

main :: IO ()
main = do
  conn <- open "app.db"
  -- findInstructors conn
  -- findTerms conn
  findClasses conn

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

findClasses :: Connection -> IO ()
findClasses conn = runBeamSqlite conn $ do
  json <- runSelectReturningList $ select $ do
    classOffering <- all_ (scheduleDb ^. scheduleClassOffering)
    course        <- related_ (scheduleDb ^. scheduleCourse)
                              (_classOfferingCourseId classOffering)
    instructor <- related_ (scheduleDb ^. scheduleInstructor)
                           (_classOfferingInstructorId classOffering)
    term <- related_ (scheduleDb ^. scheduleTerm)
                     (_classOfferingTerm classOffering)
    pure (classOffering, course, instructor, term)
  mapM_ (liftIO . print) json

findTermList :: Connection -> IO [Term]
findTermList conn = runBeamSqlite conn $ runSelectReturningList $ select
  (all_ (_scheduleTerm scheduleDb))

ft :: IO [Term]
ft = do
  conn <- open "app.db"
  runBeamSqlite conn $ runSelectReturningList $ select
    (all_ (_scheduleTerm scheduleDb))

findClassList :: IO [(ClassOffering, Course, Instructor, Term)]
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

