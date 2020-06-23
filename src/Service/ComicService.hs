module Service.ComicService where

import           Database.PostgreSQL.Simple
import Data.Int
import Data.Time.Calendar
import Data.List

import DAO.ComicDAO
import DAO.UserComicDAO 
import qualified Model.Comic as C

getUserComics :: Int64 -> Day -> Maybe Day -> Connection -> IO [C.Comic]
getUserComics idUserAccount startDate endDate conn = do 
    rsSearchTerms <- findSearchByUserId idUserAccount conn
    print rsSearchTerms >> print idUserAccount >> print startDate 
    case rsSearchTerms of 
        [] -> print "No result" >> return []
        _ -> do                       
            let mapper (Only a) = a
            let terms = mapper <$> rsSearchTerms 
            let titleClause = mconcat ["(",intercalate "|" terms,")"]    
            putStrLn titleClause
            findByTitlesAndDate titleClause startDate endDate conn
 