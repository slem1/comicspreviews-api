module Service.UserAccountService
  ( createUserAccount
  )
where

import           Database.PostgreSQL.Simple
import qualified DAO.UserAccountDAO                as UADAO
import           Model.UserAccount
import           Auth.Principal
import           Data.ByteString

createUserAccount :: UserAccount -> ByteString -> Connection -> IO UserAccount
createUserAccount account rawPassword conn =
  let password = hashPassword rawPassword
  in  UADAO.createUserAccount account password conn

