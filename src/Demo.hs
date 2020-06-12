{-# LANGUAGE OverloadedStrings #-}
module Demo (createUser) where

import qualified Model.UserAccount as UA
import qualified Service.UserAccountService as UAS
import DataSource

createUser :: String -> IO UA.UserAccount
createUser username = let user = UA.UserAccount (-1) username (username ++ "@krakoaegg.com") False in
  openConnection >>= \c -> UAS.createUserAccount user "123456" c
