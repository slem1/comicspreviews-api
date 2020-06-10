module Model.UserAccount
  ( UserAccount(..)
  )
where

import           Data.Int
import           Data.Monoid

data UserAccount = UserAccount {
    id :: Int64,
    username :: String,
    email :: String,
    enabled :: Bool
} deriving (Show)
