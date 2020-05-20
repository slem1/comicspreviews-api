{-# LANGUAGE OverloadedStrings #-}
module UserAccountServiceSpec where

import Data.Text as T
import qualified Data.Configurator as DC
import Data.Configurator.Types as DC_T
import Control.Monad.Trans.Reader
import Test.Hspec
import Test.HUnit
import Database.PostgreSQL.Simple
import PropertyUtil
import Control.Exception
import UserAccountService
import qualified UserAccount as UA
import SpecUtils

testUserAccount = UA.UserAccount { UA.id = -1, UA.username = "cyclops", UA.email = "cyclops@krakoa.com", UA.enabled = False}

spec :: Spec
spec = do 
    around withDatabaseConnection $ do     
        describe "Tests for UserAccountService module" $ do 
           it "should create account" $ withTransactionRollback $ \c -> do        
                createUserAccount testUserAccount "123456" c           
                us <- query c "SELECT username, email, enabled FROM comicspreviews.t_user_account WHERE username = ?" (Only (UA.username testUserAccount)) :: IO [(String, String, Bool)]                     
                case us of
                    [] -> expectationFailure "User not found"
                    [x] -> x `shouldBe` ("cyclops", "cyclops@krakoa.com", True)      
                    _ -> expectationFailure "No single result error"