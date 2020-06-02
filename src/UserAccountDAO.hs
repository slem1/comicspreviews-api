{-# LANGUAGE OverloadedStrings #-}

module UserAccountDAO
  ( createUserAccount
  , findByUsername
  )
where

import qualified UserAccount                   as UA
import qualified Principal                     as P
import           Database.PostgreSQL.Simple
import           Data.ByteString
import           Data.Int

createUserAccount :: UA.UserAccount -> String -> Connection -> IO UA.UserAccount
createUserAccount acc password conn = do
  [rid] <-
    query
      conn
      "INSERT INTO comicspreviews.t_user_account (username, email, enabled, password) VALUES (?, ?, ?, ?) RETURNING id_user_account"
      (UA.username acc, UA.email acc, True, password) :: IO [Only Int64]
  return $ acc { UA.id = fromOnly rid, UA.enabled = True }

findByUsername :: String -> Connection -> IO (Maybe P.Principal)
findByUsername username conn = do
  us <-
    query
      conn
      "SELECT username, enabled, password FROM comicspreviews.t_user_account WHERE username = ?"
      (Only username) :: IO [Maybe (String, Bool, String)]
  case us of
    []  -> return Nothing
    [u] -> return $ mapper <$> u
 where
  mapper (username, enabled, password) = P.Principal username enabled password
