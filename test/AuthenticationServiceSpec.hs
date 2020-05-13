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


testAccount = UA.UserAccount { UA.id = -1, UA.username = "cyclops", UA.email = "cyclops@krakoa.com", UA.enabled = False}

spec :: Spec
spec = do 
    around withDatabaseConnection $ do     
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
           it "should failed with user not found" $ withTransactionRollback $ \c -> do                                        
                authenticate "cyclops" "wrongPassword" c `shouldReturn` Left "user cyclops not found"                