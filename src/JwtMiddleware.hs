{-# LANGUAGE OverloadedStrings #-}
module JwtMiddleware
  ( proxiedApp
  , hello
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

import qualified AuthenticationService         as AuthService
import           Data.Either
import           DataSource
import qualified Data.ByteString.Char8         as BSC8
import           Lib
import           Data.Time.Clock.POSIX
import           Data.Time.Clock


hello :: Application
hello req send =
  send $ responseBuilder status200 [] "{ \"value\" : \"Hello!\"}"

myMiddleware :: Middleware
myMiddleware originalApp req send = originalApp
  req (send . mapResponseHeaders addHApplicationJson)
 where
  addHApplicationJson headers = headers ++ [(hContentType, "application/json")]

proxiedApp :: Application
proxiedApp = jwtAuthMiddleware . basicToJWTMiddleware $ hello

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
      principal = case getCredentials req of
        Nothing -> error "Cannot extract principal for Authorization header"
        Just (username, _) -> decodeUtf8 username
      addHJWT token headers = headers ++ [("auth-token", encodeUtf8 token)]
    in
      do
        token <- createJWT principal
        send $ mapResponseHeaders (addHJWT token) response
  createJWT principal = do
    secret <- getJWTSecret
    T_IO.putStrLn secret
    timeInfo <- getTokenLifeTime (3600 * 4)
    return $ AuthService.generateJWT secret principal timeInfo

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

--demo token "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.iLElXhQfr9cSxO2cYTR9wQVTd3_XMNG_pz27z3lbBF8"    


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

