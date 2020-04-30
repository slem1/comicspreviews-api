{-# LANGUAGE OverloadedStrings #-}

module UserAccountService (    
    createUserAccount,
    findByUsername
) where

import qualified UserAccount as UA
import Crypto.Hash
import Database.PostgreSQL.Simple
import Data.ByteString
import Data.Int

data Principal = Principal {
    username :: String,    
    enabled :: Bool,
    password :: String
} 

instance Show Principal where
    show (Principal u e _) = mconcat [show u, "--", show e]

createUserAccount :: UA.UserAccount -> ByteString -> Connection -> IO UA.UserAccount
createUserAccount acc rawPassword conn = withTransaction conn $ do    
    [rid] <- query conn "INSERT INTO comicspreviews.t_user_account (username, email, enabled, password) VALUES (?, ?, ?, ?) RETURNING id_user_account" (UA.username acc, UA.email acc, True, hashPassword rawPassword) :: IO [Only Int64]
    return $ acc { UA.id = fromOnly rid, UA.enabled = True}       

authenticate :: String -> ByteString -> Connection -> IO (Either String Principal)
authenticate username password conn = do
    mPrincipal <- findByUsername username conn
    return $ case mPrincipal of                                                
        Nothing -> Left . mconcat $ ["user ", username, " not found"]     
        Just principal -> verifyCredentials principal password

verifyCredentials :: Principal -> ByteString -> Either String Principal
verifyCredentials (Principal _ False _) _ = Left "user is inactive"
verifyCredentials principal inputPwd =
    if password principal == show inputPwd
    then Right principal 
    else Left "bad credentials"
      
hashPassword :: ByteString -> String
hashPassword pass = show (hashWith SHA256 pass) :: String        

findByUsername :: String -> Connection -> IO (Maybe Principal) 
findByUsername username conn = withTransaction conn $ do 
    us <- query conn "SELECT username, enabled, password FROM comicspreviews.t_user_account WHERE username = ?" (Only username) :: IO [Maybe (String, Bool, String)]
    case us of 
        [] -> return Nothing
        [u] -> return $ mapper <$> u
    where 
        mapper (username, enabled, password) = Principal username enabled password
    

