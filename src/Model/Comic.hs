{-# LANGUAGE OverloadedStrings #-}
module Model.Comic (
    Comic(..)
)
where

import Data.Aeson
import Data.Aeson.Types
import Data.Time.Calendar
import Data.Int

data Comic = Comic {
    comicId :: Int64,
    reference :: String,
    title :: String,
    price :: String,
    editor :: String,
    releaseDate :: Day
} deriving (Show, Eq)

instance ToJSON Comic where  
    toJSON (Comic comicId reference title price editor releaseDate) =
        object ["id" .= comicId, "reference" .= reference, "title" .= title, "price" .= price, "editor" .= editor, "releaseDate" .= releaseDate]
    toEncoding (Comic comicId reference title price editor releaseDate) =
        pairs ("id" .= comicId <> "reference" .= reference <> "title" .= title <> "price" .= price <> "editor" .= editor <> "releaseDate" .= releaseDate) 


instance FromJSON Comic where
    parseJSON = withObject "Comic" $ \v -> Comic
        <$> v .: "id"
        <*> v .: "reference"
        <*> v .: "title"
        <*> v .: "price"
        <*> v .: "editor"
        <*> v .: "releaseDate"