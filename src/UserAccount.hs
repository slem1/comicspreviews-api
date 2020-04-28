module UserAccount(
    UserAccount(..)
)

where

import Data.Int

data UserAccount = UserAccount {
    id :: Int64,
    username :: String,
    email :: String,
    enabled :: Bool
} deriving (Show)