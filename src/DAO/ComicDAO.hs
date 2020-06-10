{-# LANGUAGE OverloadedStrings #-}
module DAO.ComicDAO where

import Database.PostgreSQL.Simple
import Data.Time.Calendar
import Data.Int

findByTitlesAndDate :: String -> Day -> Maybe Day -> Connection -> IO [(Int64, String, String, String, Day)]
findByTitlesAndDate serie startDate endDate conn = query conn
      "SELECT tcom.id_t_comic, tcom.reference, tcom.title, tcom.editor, tcat.date_release \
      \FROM comicspreviews.t_comic tcom INNER JOIN comicspreviews.t_catalog tcat ON tcom.id_t_catalog = tcat.id_t_catalog \
      \WHERE title ~* ? AND tcat.date_release  BETWEEN ? AND COALESCE(?, tcat.date_release)"
      (serie, startDate, endDate)
