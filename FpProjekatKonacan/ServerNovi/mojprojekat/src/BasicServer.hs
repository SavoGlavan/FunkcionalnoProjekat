{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module BasicServer where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Except (throwE)
import           Data.Int (Int64)
import           Data.Proxy (Proxy(..))
import           Database.Persist (Key, Entity)
import           Database.Persist.Postgresql (ConnectionString)
import           Network.Wai.Handler.Warp (run)
import           Servant.API
import           Servant.Client
import           Servant.Server
import           Database (fetchAllMoviesPG,fetchAllMoviesForGenrePG, fetchMoviePG, createMoviePG, deleteMoviePG, localConnString,createGenrePG)
import           BasicSchema


type MoviesAPI = 
       "movies" :> Capture "movieid" Int64 :> Get '[JSON] Movie
  :<|> "movies" :> ReqBody '[JSON] Movie :> Post '[JSON] Int64
  :<|> "movies" :> "All" :> Get '[JSON] [Entity Movie]
  :<|> "movies" :> "Action" :> Get '[JSON] [Entity Movie]
  :<|> "movies" :> "Crime" :> Get '[JSON] [Entity Movie]
  :<|> "movies" :> "Drama" :> Get '[JSON] [Entity Movie]
  :<|> "movies" :> "Adventure" :> Get '[JSON] [Entity Movie]
  :<|> "movies" :> "Biography" :> Get '[JSON] [Entity Movie]
  :<|> "movies" :> "History" :> Get '[JSON] [Entity Movie]
  :<|> "movies" :> "Sci-fi" :> Get '[JSON] [Entity Movie]
  :<|> "movies" :> "Romance" :> Get '[JSON] [Entity Movie]
   :<|> "movies" :> "Western" :> Get '[JSON] [Entity Movie]
  :<|> "movies" :> "Fantasy" :> Get '[JSON] [Entity Movie]
  :<|> "movies" :> "Comedy" :> Get '[JSON] [Entity Movie]
  :<|> "movies" :> "Thriller" :> Get '[JSON] [Entity Movie]
  :<|> "movies" :> Capture "movieid" Int64 :> Post '[JSON] () 


moviesAPI :: Proxy MoviesAPI
moviesAPI = Proxy :: Proxy MoviesAPI

fetchMovieHandler :: ConnectionString -> Int64 -> Handler Movie
fetchMovieHandler connString mid = do
  maybeMovie <- liftIO $ fetchMoviePG connString mid
  case maybeMovie of
    Just movie -> return movie
    Nothing -> Handler $ (throwE $ err401 { errBody = "Could not find movie with that ID" })


fetchAllMoviesHandler :: ConnectionString -> Handler [Entity Movie]
fetchAllMoviesHandler connString = liftIO $  fetchAllMoviesPG connString

fetchAllMoviesActionHandler :: ConnectionString -> Handler [Entity Movie]
fetchAllMoviesActionHandler connString = liftIO $ fetchAllMoviesForGenrePG connString 1

fetchAllMoviesCrimeHandler :: ConnectionString -> Handler [Entity Movie]
fetchAllMoviesCrimeHandler connString = liftIO $ fetchAllMoviesForGenrePG connString 2

fetchAllMoviesDramaHandler :: ConnectionString -> Handler [Entity Movie]
fetchAllMoviesDramaHandler connString = liftIO $ fetchAllMoviesForGenrePG connString 3

fetchAllMoviesAdventureHandler :: ConnectionString -> Handler [Entity Movie]
fetchAllMoviesAdventureHandler connString = liftIO $ fetchAllMoviesForGenrePG connString 4

fetchAllMoviesBiographyHandler :: ConnectionString -> Handler [Entity Movie]
fetchAllMoviesBiographyHandler connString = liftIO $ fetchAllMoviesForGenrePG connString 5

fetchAllMoviesHistoryHandler :: ConnectionString -> Handler [Entity Movie]
fetchAllMoviesHistoryHandler connString = liftIO $ fetchAllMoviesForGenrePG connString 6

fetchAllMoviesScifiHandler :: ConnectionString -> Handler [Entity Movie]
fetchAllMoviesScifiHandler connString = liftIO $ fetchAllMoviesForGenrePG connString 7

fetchAllMoviesRomanceHandler :: ConnectionString -> Handler [Entity Movie]
fetchAllMoviesRomanceHandler connString = liftIO $ fetchAllMoviesForGenrePG connString 8

fetchAllMoviesWesternHandler :: ConnectionString -> Handler [Entity Movie]
fetchAllMoviesWesternHandler connString = liftIO $ fetchAllMoviesForGenrePG connString 9

fetchAllMoviesFantasyHandler :: ConnectionString -> Handler [Entity Movie]
fetchAllMoviesFantasyHandler connString = liftIO $ fetchAllMoviesForGenrePG connString 10

fetchAllMoviesComedyHandler :: ConnectionString -> Handler [Entity Movie]
fetchAllMoviesComedyHandler connString = liftIO $ fetchAllMoviesForGenrePG connString 11

fetchAllMoviesThrillerHandler :: ConnectionString -> Handler [Entity Movie]
fetchAllMoviesThrillerHandler connString = liftIO $ fetchAllMoviesForGenrePG connString 12

createMovieHandler :: ConnectionString -> Movie -> Handler Int64
createMovieHandler connString movie = liftIO $ createMoviePG connString movie

deleteMovieHandler :: ConnectionString -> Int64 -> Handler ()
deleteMovieHandler connString mid = liftIO $ deleteMoviePG connString mid

moviesServer :: ConnectionString -> Server MoviesAPI
moviesServer connString = 
  (fetchMovieHandler connString) :<|> 
  (createMovieHandler connString) :<|> 
  (fetchAllMoviesHandler connString) :<|>
  (fetchAllMoviesActionHandler connString) :<|>
  (fetchAllMoviesCrimeHandler connString) :<|>
  (fetchAllMoviesDramaHandler  connString) :<|>
  (fetchAllMoviesAdventureHandler connString) :<|>
  (fetchAllMoviesBiographyHandler connString) :<|>
  (fetchAllMoviesHistoryHandler connString) :<|>
  (fetchAllMoviesScifiHandler connString) :<|>
  (fetchAllMoviesRomanceHandler  connString) :<|>
  (fetchAllMoviesWesternHandler  connString) :<|>
  (fetchAllMoviesFantasyHandler  connString) :<|>
  (fetchAllMoviesComedyHandler connString) :<|>
  (fetchAllMoviesThrillerHandler connString) :<|>
  (deleteMovieHandler connString)  


runServer :: IO ()
runServer = run 5000 (serve moviesAPI (moviesServer localConnString))

