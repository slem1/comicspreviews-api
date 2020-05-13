module AuthenticationService (
    authenticate
) 
where

import UserDAO
import Principal

import Database.PostgreSQL.Simple
import Data.ByteString


authenticate :: String -> ByteString -> Connection -> IO (Either String Principal)
authenticate username rawPassword conn = do
    mPrincipal <- findByUsername username conn
    return $ case mPrincipal of                                                
        Nothing -> Left . mconcat $ ["user ", username, " not found"]     
        Just principal -> verifyCredentials principal (hashPassword rawPassword) >>= clearPassword
    where
        clearPassword p = return $ p { password = ""}


verifyCredentials :: Principal -> String-> Either String Principal
verifyCredentials (Principal _ False _) _ = Left "user is inactive"
verifyCredentials principal inputPwd =
    if password principal == inputPwd
    then Right principal 
    else Left "bad credentials"


      
 