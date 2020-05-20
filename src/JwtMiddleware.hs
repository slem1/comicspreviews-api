{-# LANGUAGE OverloadedStrings #-}
module JwtMiddleware (
    proxiedApp,
    hello
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

import qualified AuthenticationService as AuthService
import Data.Either
import DataSource
import qualified Data.ByteString.Char8 as BSC8



hello :: Application
hello req send = send $ responseBuilder status200 [] "{ \"value\" : \"Hello!\"}"

myMiddleware :: Middleware
myMiddleware originalApp req send =  originalApp req (\response -> 
    send $ mapResponseHeaders addHApplicationJson response) 
    where 
        addHApplicationJson = (\headers -> headers ++ [(hContentType, "application/json")])

proxiedApp :: Application 
proxiedApp = authMiddleware hello

jwtAuthMiddleware :: Middleware
jwtAuthMiddleware baseapp req send = case lookup "auth-token" $ requestHeaders req of
    Nothing -> send401
    Just jwt -> do 
        mJwt <- verifyJWT . decodeUtf8 $ jwt
        case mJwt of
            Nothing -> putStrLn "Not verified" >> send401
            _ -> send $ responseBuilder status200 [] "Authorized"
    where
        send401 = send $ responseBuilder status401 [] ""         

--basicAuth :: CheckCreds -> AuthSettings -> Middleware

basicAuthPath = "/auth/login"

basicAuthEntrypoint :: Middleware
basicAuthEntrypoint = basicAuth authenticate settings
    where          
        settings = "Krakoa" { authIsProtected = \req -> return $ protectedRoute req } :: AuthSettings
        protectedRoute req = case (rawPathInfo req) of
            basicAuthPath -> True
            otherwise -> False
        authenticate username password = do 
            let u = BSC8.unpack username        
            result <- openConnection >>= AuthService.authenticate u password
            case result of 
                Left err -> putStrLn err >> return False
                Right p -> return True
    
authMiddleware :: Middleware
authMiddleware originalApp = jwtMiddleware . basicAuthEntrypoint $ originalApp where
    jwtMiddleware basicAuthApplication req send = basicAuthApplication req (\response ->  do        
        if shouldReturnJWT (responseStatus response) (rawPathInfo req) 
        then sendJWT req send response
        else send $ response)       
    sendJWT req send response =        
        let getCredentials req =(lookup hAuthorization $ requestHeaders req) >>= extractBasicAuth            
            principal = case (getCredentials req) of                
                Nothing -> error "Cannot extract principal for Authorization header" 
                Just (username, _) -> decodeUtf8 username 
            addHJWT token = (\headers -> headers ++ [("auth-token", encodeUtf8 (token))])
        in do            
            token <- getJWT $ principal
            send $ mapResponseHeaders (addHJWT token) response                

shouldReturnJWT (Status code _) req
    | code == 200 && req == basicAuthPath = True
    | otherwise = False
    
--demo token "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiIxMjM0NTY3ODkwIiwibmFtZSI6IkpvaG4gRG9lIiwiaWF0IjoxNTE2MjM5MDIyfQ.iLElXhQfr9cSxO2cYTR9wQVTd3_XMNG_pz27z3lbBF8"    
    
verifyJWT :: Text -> IO (Maybe (JWT VerifiedJWT))     
verifyJWT jwt = do   
  jwtSecret <- getJWTSecret
  let secret = hmacSecret jwtSecret
  return $ decodeAndVerifySignature secret jwt

getJWT :: Text -> IO Text
getJWT principal = getJWTSecret >>= (\secret -> return $ AuthService.generateJWT secret principal)            

getJWTSecret :: IO Text
getJWTSecret = do    
    config <- DC.load [DC.Required "application.properties"] 
    secretKeyPath <- DC.require config $ pack "key_path" 
    pack <$> getEncryptedProperty config (pack "jwt_secret") secretKeyPath