module UserAccountService (
    createUserAccount
) 
where

import Database.PostgreSQL.Simple
import qualified UserAccountDAO as UADAO
import UserAccount
import Principal
import Data.ByteString

createUserAccount :: UserAccount -> ByteString -> Connection -> IO UserAccount
createUserAccount account rawPassword conn = let password = hashPassword rawPassword in 
    UADAO.createUserAccount account password conn 
    