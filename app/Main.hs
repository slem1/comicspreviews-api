{-# LANGUAGE OverloadedStrings #-}
module Main where


import Web.Scotty
import Network.HTTP.Types.Status
import Network.Wai.Middleware.HttpAuth

import qualified Data.Configurator as DC
import Data.Configurator.Types as DC_T

import Data.Monoid (mconcat)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Control.Monad.IO.Class

import Data.ByteString.UTF8
import Web.JWT

import Network.Wai.Handler.Warp
import Database.PostgreSQL.Simple
import Data.Time.Calendar
import Data.Int

import PropertyUtil
import Model.Article
import Auth.JwtMiddleware
import qualified Model.UserAccount as UA
import qualified Service.UserAccountService as UAS
import qualified Service.ComicService as ComicService
import DataSource


main = scotty 3000 $ do 
  middleware authMiddleware
  get "/auth/login" (status status200)
  get "/user/:uid/comics" $ uidAcl $ do 
    uid <- param "uid" :: ActionM Int64
    result <- liftIO $ do
      c <- openConnection 
      ComicService.getUserComics uid (fromGregorian 2020 3 4) Nothing c                  
    json result


uidAcl :: ActionM () -> ActionM ()
uidAcl action = do  
  uid <- param "uid" :: ActionM T.Text
  Just jwt <- Web.Scotty.header "auth-token"         
  let Just decodeJwt = Web.JWT.decode (TL.toStrict jwt)
      Just userId = stringOrURIToText <$> sub (claims decodeJwt) in 
      if uid == userId 
        then action
        else status status403 




{--  scotty 3000 $ do
  middleware $ basicAuth (\u p -> return $ u == "michael" && p == "mypass") "My Realm"
  
  delete "/" $ do
    html "This was a DELETE request!"  
  post "/" $ do
    text "This was a POST request!"
  put "/" $ do
    text "This was a PUT request!"  
  partJson
  part2
  --}


part2 :: ScottyM () 
part2 = do 
  post "/set-headers" $ do
    status status302
    setHeader "Location" "http://www.google.com"
  get "/askfor/:word" $ do
    w <- param "word"    
    html $ mconcat ["<h1>You asked for ", w, ", you got it !</h1>"]
  post "/submit" $ do
    name <- param "name"
    text name
  matchAny "/all" $ do
    text "matches all methods"
  notFound $ do
    text "there is no such route"

partJson :: ScottyM ()
partJson = do
  get "/article" $ do
    json $ Article 13 "caption" "content"
  post "/article" $ do
    article <- jsonData :: ActionM Article
    json article

