{-# LANGUAGE OverloadedStrings #-}
module DAO.ComicDAO where

import Database.PostgreSQL.Simple
import Data.Time.Calendar
import Data.Int
import qualified Model.Comic as C

findByTitlesAndDate :: String -> Day -> Maybe Day -> Connection -> IO [C.Comic]
findByTitlesAndDate serie startDate endDate conn = map rowMapper <$> query conn
      "SELECT tcom.id_t_comic, tcom.reference, tcom.title, tcom.editor, tcat.date_release \
      \FROM comicspreviews.t_comic tcom INNER JOIN comicspreviews.t_catalog tcat ON tcom.id_t_catalog = tcat.id_t_catalog \
      \WHERE title ~* ? AND tcat.date_release  BETWEEN ? AND COALESCE(?, tcat.date_release)"
      (serie, startDate, endDate)
      where
            rowMapper (idComic, reference, title, editor, date ) =  C.Comic { 
              C.comicId = idComic
            , C.reference = reference
            , C.title = title
            , C.price = ""
            , C.editor = editor
            , C.releaseDate= date
            }
