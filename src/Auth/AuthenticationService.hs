{-# LANGUAGE OverloadedStrings #-}
module Auth.AuthenticationService
  ( authenticate
  , generateJWT
  , verifyJWT
  , verifyExpiration
  )
where

import           DAO.UserAccountDAO
import           Auth.Principal

import           Database.PostgreSQL.Simple
import           Data.ByteString
import           Web.JWT
import           Data.Text
import           Data.Time.Clock
import qualified Data.Map.Strict as Map
import qualified Data.Aeson.Types as AesonTypes

import qualified Model.UserAccount as UserAccount


authenticate
  :: String -> ByteString -> Connection -> IO (Either String Principal)
authenticate username rawPassword conn =
  let password = hashPassword rawPassword
  in  do
        mPrincipal <- findByUsername username conn
        return $ case mPrincipal of
          Nothing -> Left . mconcat $ ["user ", username, " not found"]
          Just principal ->
            verifyCredentials principal password >>= clearPassword
  where clearPassword p = return $ p { password = "" }


verifyCredentials :: Principal -> String -> Either String Principal
verifyCredentials (Principal _ _ False _) _ = Left "user is inactive"
verifyCredentials principal inputPwd = if password principal == inputPwd
  then Right principal
  else Left "bad credentials"


generateJWT :: Text -> UserAccount.UserAccount -> (Maybe NumericDate, Maybe NumericDate) -> Text
generateJWT secret user (time, expiry) =
  let uid = Data.Text.pack . show $ UserAccount.id user  
      username = AesonTypes.String . Data.Text.pack  $ UserAccount.username user
      additionalClaims = ClaimsMap $ Map.singleton (Data.Text.pack "username") username
      key     = hmacSecret secret
      content = mempty { iss         = stringOrURI "comicpreviews-api"
                       , sub         = stringOrURI uid
                       , iat         = time
                       , Web.JWT.exp = expiry
                       , unregisteredClaims = additionalClaims
                       }
  in  encodeSigned key mempty content

verifyJWT :: Text -> Maybe NumericDate -> Text -> Maybe (JWT VerifiedJWT)
verifyJWT jwtSecret currentTime token =
  let secret = hmacSecret jwtSecret
  in  do
        time <- currentTime
        jwt  <- decodeAndVerifySignature secret token
        verifyExpiration time jwt
        return jwt

verifyExpiration :: NumericDate -> JWT VerifiedJWT -> Maybe NumericDate
verifyExpiration time jwt =
  Web.JWT.exp (claims jwt) >>= \exp -> if time > exp then Nothing else Just exp
