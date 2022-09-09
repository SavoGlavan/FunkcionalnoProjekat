{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}

module BasicSchema where

import           Data.Aeson
import           Data.Aeson.Types
import           Database.Persist (Entity(..), Entity)
import           Database.Persist.Sql (fromSqlKey, toSqlKey)
import qualified Database.Persist.TH as PTH
import           Data.Text (Text)


PTH.share [PTH.mkPersist PTH.sqlSettings, PTH.mkMigrate "migrateAll"] [PTH.persistLowerCase|
  Genre sql=genre
    name Text
    UniqueTitle name
    deriving Show Read Eq

  Movie sql=movies
    poster Text
    title Text
    UniqueText title
    year Int
    rating_imdb Double
    genreId GenreId
    director Text
    stars Text
   
    
    deriving Show Read Eq
|]
instance ToJSON (Entity Movie) where
  toJSON (Entity mid movie) = object $
    "id" .= (fromSqlKey mid) : moviePairs movie
instance ToJSON Movie where
    toJSON movie = object (moviePairs movie)
moviePairs :: Movie -> [Pair]
moviePairs movie =
  [ 
    "Poster_Link" .= moviePoster movie
  , "Series_Title" .= movieTitle movie
  , "Released_Year" .= movieYear movie 
  , "IMDB_Rating" .= movieRating_imdb movie
  , "GenreId" .= movieGenreId movie
  , "Director" .= movieDirector movie
  , "Stars" .= movieStars movie
  
  ]

instance FromJSON (Entity Movie) where
  parseJSON = withObject "Movie Entity" $ \o -> do
    movie <- parseMovie o
    mid <- o .: "id"
    return $ Entity (toSqlKey mid) movie

instance FromJSON Movie where
  parseJSON = withObject "Movie" parseMovie

parseMovie :: Object -> Parser Movie
parseMovie o = do
  uPoster <- o .: "Poster_Link"
  uTitle <- o .: "Series_Title"
  uYear <- o .: "Released_Year"
  uRatingimdb <- o .: "IMDB_Rating"
  uDirector <- o .: "Director"
  uStars <- o .: "Stars"
  uGenreId <- o .: "Genre"
 
  return Movie
    { moviePoster = uPoster
    , movieTitle = uTitle
    , movieYear = uYear
    , movieRating_imdb = uRatingimdb
    , movieGenreId = uGenreId
    , movieDirector = uDirector
    , movieStars = uStars
    }



instance ToJSON (Entity Genre) where
  toJSON (Entity gid genre) = object $
    "id" .= (fromSqlKey gid) : genrePairs genre

instance ToJSON Genre where
  toJSON genre = object (genrePairs genre)

genrePairs :: Genre -> [Pair]
genrePairs genre =
  [ "name" .= genreName genre
  ]

instance FromJSON (Entity Genre) where
  parseJSON = withObject "Genre Entity" $ \o -> do
    genre <- parseGenre o
    gid <- o .: "id"
    return $ Entity (toSqlKey gid) genre

--
instance FromJSON Genre where
  parseJSON = withObject "Genre" parseGenre

parseGenre :: Object -> Parser Genre
parseGenre o = do
  uName <- o .: "name"
  return Genre
    { genreName = uName
    }