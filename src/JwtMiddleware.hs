{-# LANGUAGE OverloadedStrings #-}
module JwtMiddleware (
    proxiedApp
 ) where

import Network.Wai
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header

import PropertyUtil
import Web.JWT
import Network.Wai.Middleware.HttpAuth


import qualified Data.Configurator as DC
import qualified Data.Configurator.Types as DC_T
import qualified Data.ByteString as BS
import Data.Text
import Data.Text.Encoding




hello :: Application
hello req send = send $ responseBuilder status200 [] "{ \"value\" : \"Hello!\"}"

myMiddleware :: Middleware
myMiddleware originalApp req send =  originalApp req (\response -> 
    send $ mapResponseHeaders addHApplicationJson response) 
    where 
        addHApplicationJson = (\headers -> headers ++ [(hContentType, "application/json")])

proxiedApp :: Application 
proxiedApp = jwtAuthMiddleware hello

jwtAuthMiddleware :: Middleware
jwtAuthMiddleware baseapp req send = case lookup "auth-token" $ requestHeaders req of
    Nothing -> send401
    Just jwt -> do 
        mJwt <- verifyJwt . decodeUtf8 $ jwt
        case mJwt of
            Nothing -> putStrLn "Not verified" >> send401
            _ -> send $ responseBuilder status200 [] "Authorized"
    where
        send401 = send $ responseBuilder status401 [] ""         

--basicAuth :: CheckCreds -> AuthSettings -> Middleware

basicAuthEntrypoint = basicAuth (\u p -> return $ u == "michael" && p == "mypass") settings
    where          
        settings = "Krakoa" { authIsProtected = \req -> return $ protectedRoute req } :: AuthSettings
        protectedRoute req = case (rawPathInfo req) of
            "/auth/login" -> True
            otherwise -> False
    


    
--demo token "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.iLElXhQfr9cSxO2cYTR9wQVTd3_XMNG_pz27z3lbBF8"    
    
verifyJwt :: Text -> IO (Maybe (JWT VerifiedJWT))     
verifyJwt jwt = do 
  config <- DC.load [DC.Required "application.properties"] 
  secretKeyPath <- DC.require config $ pack "key_path" 
  jwtSecret <- getEncryptedProperty config (pack "jwt_secret") secretKeyPath
  let secret = hmacSecret (pack jwtSecret)
  return $ decodeAndVerifySignature secret jwt