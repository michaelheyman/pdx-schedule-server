{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}         -- allTerms
{-# LANGUAGE AllowAmbiguousTypes #-}  -- allTerms

module Server where

import           Prelude                        ( )
import           Prelude.Compat

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Attoparsec.ByteString
import           Data.ByteString                ( ByteString )
import           Data.List
import           Data.Maybe
import           Data.String.Conversions
import           Data.Time.Calendar
import           GHC.Generics
--import Lucid
import           Network.HTTP.Media             ( (//)
                                                , (/:)
                                                )
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           System.Directory
import           Text.Blaze
import           Text.Blaze.Html.Renderer.Utf8
import           Servant.Types.SourceT          ( source )
import qualified Data.Aeson.Parser
import qualified Text.Blaze.Html
import           Schema
import           Database.Beam.Backend.SQL.SQL92
                                                ( IsSql92Syntax
                                                , Sql92SanityCheck
                                                )
import           Control.Lens                   ( (^.) )

import           Database.Beam

import           Database.Beam.Sqlite
import           Database.SQLite.Simple         ( open
                                                , Connection
                                                )

type TermAPI = "terms" :> Get '[JSON] [Term]
type ClassOfferingAPI = "classes" :> Get '[JSON] [ClassOffering]
type CourseAPI = "courses" :> Get '[JSON] [Course]
type InstructorAPI = "classes" :> Get '[JSON] [Instructor]

server1 :: Server TermAPI
-- server1 = return terms1
server1 = liftIO fullTermList

server2 :: Server ClassOfferingAPI
server2 = liftIO fullClassList

-- server3 :: Server (ClassOfferingAPI, CourseAPI, InstructorAPI, TermAPI)
-- server3 = liftIO findClassList

termAPI :: Proxy TermAPI
termAPI = Proxy :: Proxy TermAPI

classAPI :: Proxy ClassOfferingAPI
classAPI = Proxy :: Proxy ClassOfferingAPI

allAPI :: Proxy (ClassOfferingAPI, CourseAPI, InstructorAPI, TermAPI)
allAPI = Proxy :: Proxy (ClassOfferingAPI, CourseAPI, InstructorAPI, TermAPI)


app1 :: Application
app1 = serve termAPI server1

app2 :: Application
app2 = serve classAPI server2

-- app3 :: Application
-- app3 = serve allAPI server3

main :: IO ()
main = do
  putStrLn "Running server on http://localhost:8081/terms"
  run 8081 app2

-- messages :: ( IsSql92Syntax cmd, Sql92SanityCheck cmd, MonadBeam cmd be hdl m) => Connection -> Handler [Term]
-- messages :: Connection -> Handler [Term]
-- messages conn = do 
--   messages <- liftIO $ runSelectReturningList $ select (all_ (_scheduleTerm scheduleDb))
--   liftIO . putStrLn . show . encode . getProps $ messages
--   return messages

getAllTerms :: ReaderT Connection Handler [Term]
getAllTerms = do
  conn <- ask
  liftIO
    $ runBeamSqliteDebug putStrLn conn
    $ runSelectReturningList
    $ select
    $ all_ (_scheduleTerm scheduleDb)


-- allTerms :: Connection -> Handler [Term]
-- allTerms :: ( IsSql92Syntax cmd, 
--               Sql92SanityCheck cmd, 
--               MonadBeam cmd be hdl m) => Connection -> Handler [Term]
-- allTerms :: ( IsSql92Syntax cmd, 
--               Sql92SanityCheck cmd, 
--               MonadBeam cmd be hdl m) => Connection -> [TermT Identity]
-- allTerms = do
--   terms <- liftIO $ runSelectReturningList $ select (all_ (_scheduleTerm scheduleDb))
--   -- liftIO . putStrLn . show . encode . getProps $ terms
--   pure terms

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
