{-# LANGUAGE OverloadedStrings #-}

module Database where

import           Control.Monad.Logger
import           Control.Monad.Reader (runReaderT)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Int (Int64)
import           Database.Persist hiding ((==.))
import           Database.Persist.Postgresql hiding ((==.))
import           Database.Esqueleto (select, from, where_, (^.), val, (==.), on,
                                     InnerJoin(..), limit, orderBy, desc)

import BasicSchema

type PGInfo = ConnectionString

localConnString :: PGInfo
localConnString = "host=127.0.0.1 port=5432 user=postgres dbname=moviesNew password=zvezda99"

runAction :: PGInfo -> SqlPersistT (LoggingT IO) a -> IO a
runAction connectionString action = 
  runStdoutLoggingT $ filterLogger logFilter $ withPostgresqlConn connectionString $ \backend ->
   runReaderT action backend

migrateDB :: PGInfo -> IO ()
migrateDB connString = runAction connString (runMigration migrateAll)

logFilter :: a -> LogLevel -> Bool
logFilter _ LevelError     = True
logFilter _ LevelWarn      = True
logFilter _ LevelInfo      = True
logFilter _ LevelDebug     = False
logFilter _ (LevelOther _) = False


fetchMoviePG :: PGInfo -> Int64 -> IO (Maybe Movie)
fetchMoviePG connString mid = runAction connString (get (toSqlKey mid))


fetchAllMoviesForGenrePG :: PGInfo -> Int64  -> IO [Entity Movie]
fetchAllMoviesForGenrePG connString gid = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Movie]
    fetchAction = select . from $ \movies2 -> do
      where_ (movies2 ^. MovieGenreId ==. val (toSqlKey gid))
      return movies2


fetchAllMoviesPG :: PGInfo -> IO [Entity Movie]
fetchAllMoviesPG connString = runAction connString fetchAction
  where
    fetchAction :: SqlPersistT (LoggingT IO) [Entity Movie]
    fetchAction = select . from $ \movie2 -> do
      return movie2

createMoviePG :: PGInfo -> Movie -> IO Int64 
createMoviePG connString movie = fromSqlKey <$> runAction connString (insert movie)

deleteMoviePG :: PGInfo -> Int64 -> IO () 
deleteMoviePG connString mid = runAction connString (delete movieKey)
  where
    movieKey :: Key Movie
    movieKey = toSqlKey mid

createGenrePG :: PGInfo -> Genre  -> IO Int64
createGenrePG connString genre = fromSqlKey <$> runAction connString (insert genre)