{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module UserComicDAO (
    findSearchByUserId,
    addSearch
) where

import           Database.PostgreSQL.Simple
import           Data.Int

findSearchByUserId :: Int64 -> Connection -> IO [Only String]
findSearchByUserId userId conn = query conn
      "SELECT search \
      \FROM comicspreviews.t_user_search \
      \WHERE id_t_user_account = ? " 
      (Only userId)

addSearch :: Int64 -> [String] -> Connection -> IO Int64
addSearch userId terms conn = 
    let tuples = map (userId,) terms in
    executeMany conn "INSERT INTO comicspreviews.t_user_search(id_t_user_account, search) VALUES (?,?)" tuples