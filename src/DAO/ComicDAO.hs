{-# LANGUAGE OverloadedStrings #-}
module DAO.ComicDAO where

import Database.PostgreSQL.Simple
import Data.Time.Calendar
import Data.Int
import qualified Model.Comic as C

rowMapper (idComic, reference, title, editor, date ) =  C.Comic { 
  C.comicId = idComic
, C.reference = reference
, C.title = title
, C.price = ""
, C.editor = editor
, C.releaseDate= date
}


findByTitlesAndDate :: String -> Day -> Maybe Day -> Connection -> IO [C.Comic]
findByTitlesAndDate serie startDate endDate conn = map rowMapper <$> query conn
      "SELECT tcom.id_t_comic, tcom.reference, tcom.title, tcom.editor, tcat.date_release \
      \FROM comicspreviews.t_comic tcom INNER JOIN comicspreviews.t_catalog tcat ON tcom.id_t_catalog = tcat.id_t_catalog \
      \WHERE title ~* ? AND tcat.date_release  BETWEEN ? AND COALESCE(?, tcat.date_release) \
      \ORDER BY tcom.id_t_comic"
      (serie, startDate, endDate)      

findByTitlesAndDatePaginated :: String -> Day -> Maybe Day -> (Int, Int) -> Connection -> IO [C.Comic]
findByTitlesAndDatePaginated titles startDate endDate (fromId, pageSize) conn = map rowMapper <$> query conn
      "SELECT tcom.id_t_comic, tcom.reference, tcom.title, tcom.editor, tcat.date_release \
      \FROM comicspreviews.t_comic tcom INNER JOIN comicspreviews.t_catalog tcat ON tcom.id_t_catalog = tcat.id_t_catalog \
      \WHERE tcom.id_t_comic > ? AND title ~* ? AND tcat.date_release  BETWEEN ? AND COALESCE(?, tcat.date_release) \
      \ORDER BY tcom.id_t_comic \
      \LIMIT ?"
      (fromId, titles, startDate, endDate, pageSize)
