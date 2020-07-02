{-# LANGUAGE OverloadedStrings #-}
module Auth.JwtMiddleware
  ( authMiddleware
  )
where

import           Network.Wai
import           Network.HTTP.Types.Status
import           Network.HTTP.Types.Header

import           PropertyUtil
import           Web.JWT
import           Network.Wai.Middleware.HttpAuth


import qualified Data.Configurator             as DC
import qualified Data.Configurator.Types       as DC_T
import qualified Data.ByteString               as BS
import           Data.Text
import qualified Data.Text.IO                  as T_IO
import           Data.Text.Encoding

import           Data.Either
import           DataSource
import qualified Data.ByteString.Char8         as BSC8
import           Data.Time.Clock.POSIX
import           Data.Time.Clock

import qualified Auth.AuthenticationService         as AuthService
import qualified Service.UserAccountService         as UserAccountService


authMiddleware :: Middleware
authMiddleware = jwtAuthMiddleware . basicToJWTMiddleware

jwtAuthMiddleware :: Middleware
jwtAuthMiddleware baseapp req send =
  let guard mJWT = case mJWT of
        Nothing -> send401
        _       -> baseapp req send
      authToken = lookup "auth-token" $ requestHeaders req
      path      = rawPathInfo req
  in  if path == basicAuthPath
        then baseapp req send
        else case authToken of
          Nothing  -> putStrLn "Authentication token not found" >> send401
          Just jwt -> do
            secret      <- getJWTSecret
            currentTime <- getPOSIXTime'
            let check =
                  AuthService.verifyJWT secret currentTime $ decodeUtf8 jwt
            guard check
  where send401 = send $ responseBuilder status401 [] ""


basicToJWTMiddleware :: Middleware
basicToJWTMiddleware =
  jwtMiddleware . basicAuthEntrypoint where
  jwtMiddleware basicAuthApplication req send = basicAuthApplication
    req
    (\response -> 
      if shouldReturnJWT (responseStatus response) (rawPathInfo req)
        then sendJWT req send response
        else send response
    )
  sendJWT req send response =
    let
      getCredentials req =
        lookup hAuthorization (requestHeaders req) >>= extractBasicAuth
      username = case getCredentials req of
        Nothing -> error "Cannot extract principal for Authorization header"
        Just (username', _) -> unpack $ decodeUtf8 username'
      asCookie token = encodeUtf8 (mconcat ["auth-token=" ,token])
      addHJWT token headers = headers ++ [("Set-Cookie", asCookie token )]
    in
      do
        Just user <- openConnection >>= UserAccountService.getByUsername username
        token <- createJWT user        
        send $ mapResponseHeaders (addHJWT token) response
  createJWT user = do
    secret <- getJWTSecret    
    timeInfo <- getTokenLifeTime (3600 * 4)    
    return $ AuthService.generateJWT secret user timeInfo

shouldReturnJWT (Status code _) req | code == 200 && req == basicAuthPath = True
                                    | otherwise = False

basicAuthPath = "/auth/login"

basicAuthEntrypoint :: Middleware
basicAuthEntrypoint = basicAuth authenticate settings
 where
  settings =
    "Krakoa" { authIsProtected = return . protectedRoute } :: AuthSettings
  protectedRoute req = rawPathInfo req == basicAuthPath
  authenticate username password = do
    let u = BSC8.unpack username
    result <- openConnection >>= AuthService.authenticate u password
    case result of
      Left  err -> putStrLn err >> return False
      Right p   -> return True


getJWTSecret :: IO Text
getJWTSecret = do
  config        <- DC.load [DC.Required "application.properties"]
  secretKeyPath <- DC.require config $ pack "key_path"
  pack <$> getEncryptedProperty config (pack "jwt_secret") secretKeyPath

getTokenLifeTime durationInS = do
  time <- getPOSIXTime
  let exp = time + secondsToNominalDiffTime durationInS
  return (numericDate time, numericDate exp)

getPOSIXTime' :: IO (Maybe NumericDate)
getPOSIXTime' = getPOSIXTime >>= \time -> return $ numericDate time

