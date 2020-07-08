module Service.ComicService where

import           Database.PostgreSQL.Simple
import           Data.Int
import           Data.Time.Calendar
import           Data.List

import DAO.ComicDAO
import DAO.UserComicDAO 
import qualified Model.Comic as C

getUserComics :: Int64 -> Day -> Maybe Day -> Connection -> IO [C.Comic]
getUserComics idUserAccount startDate endDate conn = do     
    rsSearchTerms <- findSearchByUserId idUserAccount conn    
    case rsSearchTerms of 
        [] -> return []
        _ -> findByTitlesAndDate (titlesRegex rsSearchTerms) startDate endDate conn

getUserComicsPaginated :: Int64 -> Day -> Maybe Day -> (Int, Int) -> Connection -> IO [C.Comic]
getUserComicsPaginated idUserAccount startDate endDate paginationInfo conn = do 
    rsSearchTerms <- findSearchByUserId idUserAccount conn    
    case rsSearchTerms of 
        [] -> return []
        _ -> findByTitlesAndDatePaginated (titlesRegex rsSearchTerms) startDate endDate paginationInfo conn
 
titlesRegex :: [Only String] -> String 
titlesRegex searchTerms =     
    let mapper (Only a) = a
        terms = mapper <$> searchTerms in
            mconcat ["(",intercalate "|" terms,")"]                