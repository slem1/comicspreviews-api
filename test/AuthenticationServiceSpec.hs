{-# LANGUAGE OverloadedStrings #-}
module AuthenticationServiceSpec where

import Data.Text as T
import qualified Data.Configurator as DC
import Data.Configurator.Types as DC_T
import Control.Monad.Trans.Reader
import Test.Hspec
import Test.HUnit
import Database.PostgreSQL.Simple
import PropertyUtil
import Control.Exception
import AuthenticationService
import UserAccountService
import qualified UserAccount as UA
import SpecUtils
import Principal
import Data.Int
import Web.JWT

token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJjeWNsb3BzIiwiZXhwIjoxNTkwNzAwMzMyLCJpc3MiOiJjb21pY3ByZXZpZXdzLWFwaSIsImlhdCI6MTU5MDY4NTkzMn0.ACgyoFAZP6NsuqUCEu-iR4q8uEM1gav5vhH1QwM-njU"
invalidSignatureToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJjeWNsb3BzIiwiZXhwIjoxNTkwNzAwMzMyLCJpc3MiOiJjb21pY3ByZXZpZXdzLWFwaSIsImlhdCI6MTU5MDY4NTkzMn0.ACgyoFAZP6NsuqUCEu-iR4q8uEM1gav5vhH1QwM-XXX"
secret = "196C84272D54BB4F9183DACFB8EB2"

testAccount = UA.UserAccount { UA.id = -1, UA.username = "cyclops", UA.email = "cyclops@krakoa.com", UA.enabled = False}

spec :: Spec
spec = do 
    around withDatabaseConnection $      
        describe "Tests for AuthenticationServiceSpec module" $ do 
           it "should authenticate" $ withTransactionRollback $ \c -> do        
                let password = hashPassword "123456"
                query c "INSERT INTO comicspreviews.t_user_account (username, email, enabled, password) VALUES (?, ?, ?, ?) RETURNING id_user_account" (UA.username testAccount, UA.email testAccount, True, password) :: IO [Only Int64]               
                result <- authenticate "cyclops" "123456" c
                case result of
                    Left err -> expectationFailure err
                    Right principal -> principal `shouldBe` Principal "cyclops" True ""
           it "should failed with bad credentials" $ withTransactionRollback $ \c -> do        
                let password = hashPassword "123456"
                query c "INSERT INTO comicspreviews.t_user_account (username, email, enabled, password) VALUES (?, ?, ?, ?) RETURNING id_user_account" (UA.username testAccount, UA.email testAccount, True, password) :: IO [Only Int64]                               
                authenticate "cyclops" "wrongPassword" c `shouldReturn` Left "bad credentials"
           it "should failed with user not found" $ withTransactionRollback $ \c ->                                         
                authenticate "cyclops" "wrongPassword" c `shouldReturn` Left "user cyclops not found"      
    describe "Tests for AuthenticationServiceSpec module" $ do                 
           it "should valid JWT" $  
                let jwt = verifyJWT secret (numericDate 1590683897) token in
                     case jwt of
                          Nothing -> expectationFailure $ show jwt ++ " is invalid"
                          Just _ -> return ()
           it "should invalid JWT signature" $  
                let jwt = verifyJWT secret (numericDate 1590683897) invalidSignatureToken in
                     case jwt of
                          Nothing -> return ()
                          _ -> expectationFailure $ show jwt ++ " is valid"
           it "should invalid JWT due to expiration" $ 
                let jwt = verifyJWT secret (numericDate 1590700333) token in
                     case jwt of
                          Nothing -> return ()
                          _ -> expectationFailure $ show jwt ++ " is valid"    
                     
                       


                    