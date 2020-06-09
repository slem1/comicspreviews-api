{-# LANGUAGE OverloadedStrings #-}
module Main where


import Web.Scotty
import Network.HTTP.Types.Status
import Network.Wai.Middleware.HttpAuth

import qualified Data.Configurator as DC
import Data.Configurator.Types as DC_T

import Data.Monoid (mconcat)
import qualified Data.Text as T

import Data.ByteString.UTF8
import Web.JWT

import Network.Wai.Handler.Warp
import Database.PostgreSQL.Simple

import PropertyUtil
import Model.Article
import Auth.JwtMiddleware
import qualified Model.UserAccount as UA
import qualified Service.UserAccountService as UAS
import DataSource

around :: IO a -> (a -> IO ()) -> (a -> IO c) -> IO c
around first last op = do  
  p <- first   
  result <- op p
  last p
  return result

main =  --around getLine putStrLn (\input -> putStrLn (input ++ "modified by op"))
  do
  let user = UA.UserAccount { UA.id= -1, UA.username = "cyclops", UA.email = "cyclops@krakoa.com", UA.enabled = False }
  --openConnection >>= (\c -> withTransaction c $ UAS.createUserAccount user "123456" c)
  run 3300 proxiedApp
 
{--  scotty 3000 $ do
  middleware $ basicAuth (\u p -> return $ u == "michael" && p == "mypass") "My Realm"
  get "/" $ do     
    text "This was a GET request!"     
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

