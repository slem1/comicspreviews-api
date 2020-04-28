{-# LANGUAGE OverloadedStrings #-}

module UserAccountService (    
    createUserAccount
) where

import qualified UserAccount as UA
import Crypto.Hash
import Database.PostgreSQL.Simple
import Data.ByteString
import Data.Int

createUserAccount :: UA.UserAccount -> ByteString -> Connection -> IO UA.UserAccount
createUserAccount acc rawPassword conn = withTransaction conn $ do    
    [rid] <- query conn "INSERT INTO comicspreviews.user_account ('username', 'email', 'enabled', 'password') RETURNING id_user_account" (UA.username acc, UA.email acc, True, password rawPassword) :: IO [Only Int64]
    return $ acc { UA.id = fromOnly rid}    
    where
        password :: ByteString -> String
        password pass = show (hashWith SHA256 pass) :: String        


