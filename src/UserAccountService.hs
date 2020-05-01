module UserAccountService (
    createUserAccount
) 
where

import Database.PostgreSQL.Simple
import qualified UserDAO as UserDAO
import UserAccount
import Principal
import Data.ByteString

createUserAccount :: UserAccount -> ByteString -> Connection -> IO UserAccount
createUserAccount account rawPassword conn = let password = hashPassword rawPassword in
    withTransaction conn $ UserDAO.createUserAccount account password conn 
    