{-# LANGUAGE OverloadedStrings #-}
module Main where

import Lib
import Article

import Web.Scotty

import Data.Monoid (mconcat)
import Network.HTTP.Types.Status

main = scotty 3000 $ do
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
