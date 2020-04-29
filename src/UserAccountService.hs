{-# LANGUAGE OverloadedStrings #-}

module UserAccountService (    
    createUserAccount,
    findByUsername
) where

import qualified UserAccount as UA
import Crypto.Hash
import Database.PostgreSQL.Simple
import Data.ByteString
import Data.Int

createUserAccount :: UA.UserAccount -> ByteString -> Connection -> IO UA.UserAccount
createUserAccount acc rawPassword conn = withTransaction conn $ do    
    [rid] <- query conn "INSERT INTO comicspreviews.t_user_account (username, email, enabled, password) VALUES (?, ?, ?, ?) RETURNING id_user_account" (UA.username acc, UA.email acc, True, password rawPassword) :: IO [Only Int64]
    return $ acc { UA.id = fromOnly rid, UA.enabled = True}    
    where
        password :: ByteString -> String
        password pass = show (hashWith SHA256 pass) :: String        


findByUsername :: String -> Connection -> IO (Maybe UA.UserAccount) 
findByUsername username conn = withTransaction conn $ do 
    us <- query conn "SELECT id_user_account, username, email, enabled, password FROM comicspreviews.t_user_account WHERE username = ?" (Only username) :: IO [Maybe (Int64, String, String, Bool, String)]
    case us of 
        [] -> return Nothing
        [u] -> return $ mapper <$> u
    where 
        mapper (id, username, email, enabled, password) = UA.UserAccount id username email enabled password
    

