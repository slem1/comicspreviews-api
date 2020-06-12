{-# LANGUAGE OverloadedStrings #-}

module DAO.UserAccountDAO
  ( createUserAccount
  , findByUsername
  , findByUsername'
  )
where

import qualified Model.UserAccount                   as UA
import qualified Auth.Principal                     as P
import           Database.PostgreSQL.Simple
import           Data.ByteString
import           Data.Int

createUserAccount :: UA.UserAccount -> String -> Connection -> IO UA.UserAccount
createUserAccount acc password conn = do
  [rid] <-
    query
      conn
      "INSERT INTO comicspreviews.t_user_account (username, email, enabled, password) VALUES (?, ?, ?, ?) RETURNING id_t_user_account"
      (UA.username acc, UA.email acc, True, password) :: IO [Only Int64]
  return $ acc { UA.id = fromOnly rid, UA.enabled = True }

findByUsername :: String -> Connection -> IO (Maybe P.Principal)
findByUsername username conn = do
  us <-
    query
      conn
      "SELECT id_t_user_account, username, enabled, password FROM comicspreviews.t_user_account WHERE username = ?"
      (Only username) :: IO [Maybe (Int64, String, Bool, String)]
  case us of
    []  -> return Nothing
    [u] -> return $ mapper <$> u
 where
  mapper (uid, username, enabled, password) = P.Principal uid username enabled password

findByUsername' :: String -> Connection -> IO (Maybe UA.UserAccount)
findByUsername' username conn = do
  us <-
    query
      conn
      "SELECT id_t_user_account, username, email, enabled FROM comicspreviews.t_user_account WHERE username = ?"
      (Only username) :: IO [Maybe (Int64, String, String, Bool)]
  case us of
    []  -> return Nothing
    [u] -> return $ mapper <$> u
 where
  mapper (uid, username, email, enabled) = UA.UserAccount uid username email enabled 
