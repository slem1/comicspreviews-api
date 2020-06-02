{-# LANGUAGE OverloadedStrings #-}

module Article
  ( Article(..)
  )
where

import           Data.Text.Lazy
import           Data.Text.Lazy.Encoding
import           Data.Aeson
import           Control.Applicative

data Article = Article Integer Text Text deriving (Show)

instance FromJSON Article where
  parseJSON (Object v) =
    Article <$> v .:? "id" .!= 0 <*> v .: "title" <*> v .: "bodyText"

instance ToJSON Article where
  toJSON (Article id title bodyText) =
    object ["id" .= id, "title" .= title, "bodyText" .= bodyText]
