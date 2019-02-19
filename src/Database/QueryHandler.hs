module Database.QueryHandler where

import           Control.Monad.Reader   (ReaderT, ask, liftIO)
import           Database.Beam          (all_, runSelectReturningList, select)
import           Database.Beam.Sqlite   (runBeamSqliteDebug)
import           Database.Schema
import           Database.SQLite.Simple (Connection)
import           Servant

getAllTerms :: ReaderT Connection Handler [Term]
getAllTerms = do
  conn <- ask
  liftIO
    $ runBeamSqliteDebug putStrLn conn
    $ runSelectReturningList
    $ select
    $ all_ (_scheduleTerm scheduleDb)
