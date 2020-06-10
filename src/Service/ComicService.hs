module Service.ComicService where

import           Database.PostgreSQL.Simple
import Data.Int
import Data.Time.Calendar
import Data.List

import DAO.ComicDAO
import DAO.UserComicDAO 

getUserComics :: Int64 -> Day -> Maybe Day -> Connection -> IO [(Int64, String, String, String, Day)]
getUserComics idUserAccount startDate endDate conn = do 
    rsSearchTerms <- findSearchByUserId idUserAccount conn
    case rsSearchTerms of 
        [] -> return []
        _ -> do                       
            let mapper (Only a) = a
            let terms = mapper <$> rsSearchTerms 
            let titleClause = mconcat ["(",intercalate "|" terms,")"]    
            findByTitlesAndDate titleClause startDate endDate conn
 