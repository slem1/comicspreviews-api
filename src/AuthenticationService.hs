{-# LANGUAGE OverloadedStrings #-}
module AuthenticationService (
    authenticate,
    generateJWT,
    getTime
) 
where

import UserAccountDAO
import Principal

import Database.PostgreSQL.Simple
import Data.ByteString
import Web.JWT
import Data.Text
import Data.Time.Clock.System
import Data.Time.Clock.POSIX


authenticate :: String -> ByteString -> Connection -> IO (Either String Principal)
authenticate username rawPassword conn = let password = hashPassword rawPassword in do    
    mPrincipal <- findByUsername username conn
    return $ case mPrincipal of                                                
        Nothing -> Left . mconcat $ ["user ", username, " not found"]     
        Just principal -> verifyCredentials principal password >>= clearPassword
    where
        clearPassword p = return $ p { password = ""}


verifyCredentials :: Principal -> String-> Either String Principal
verifyCredentials (Principal _ False _) _ = Left "user is inactive"
verifyCredentials principal inputPwd =
    if password principal == inputPwd
    then Right principal 
    else Left "bad credentials"


generateJWT :: Text -> Text -> Text
generateJWT secret principal = 
    let
        key = hmacSecret secret 
        content = mempty {
            iss =  stringOrURI "comicpreviews-api",
            sub =  stringOrURI principal
        }
    in encodeSigned key mempty content

      
getTime = getPOSIXTime >>= \time -> Prelude.putStrLn $ show $ numericDate (time)