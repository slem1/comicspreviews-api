{-# LANGUAGE OverloadedStrings #-}
module UserComicDAOSpec where

import Data.Text as T
import qualified Data.Configurator as DC
import Data.Configurator.Types as DC_T
import Control.Monad.Trans.Reader
import Test.Hspec
import Test.HUnit
import Database.PostgreSQL.Simple

import Control.Exception
import SpecUtils

import Data.Int

import PropertyUtil
import DAO.UserComicDAO
import qualified Model.UserAccount as UA

testAccount = UA.UserAccount { UA.id = -1, UA.username = "wolverine", UA.email = "wolverine@krakoa.com", UA.enabled = False}


spec :: Spec
spec =  
    around withDatabaseConnection $      
        describe "Tests for UserComicDAO module" $ do
           it "should find user searches" $ withTransactionRollback $ \c -> do                        
                [Only uid] <- query c "INSERT INTO comicspreviews.t_user_account (username, email, enabled, password) VALUES (?, ?, ?, ?) RETURNING id_t_user_account" (UA.username testAccount, UA.email testAccount, True, "password" :: String)                
                execute c "INSERT INTO comicspreviews.t_user_search (id_t_user_account, search) VALUES (?, 'BLACK CAT')" (Only uid)
                execute c "INSERT INTO comicspreviews.t_user_search (id_t_user_account, search) VALUES (?, 'X-MEN')" (Only uid)
                [Only r1, Only r2] <- findSearchByUserId uid c       
                (r1,r2) `shouldBe` ("BLACK CAT", "X-MEN")
           it "should add search terms" $ withTransactionRollback $ \c -> do                        
                [Only uid] <- query c "INSERT INTO comicspreviews.t_user_account (username, email, enabled, password) VALUES (?, ?, ?, ?) RETURNING id_t_user_account" (UA.username testAccount, UA.email testAccount, True, "password" :: String)                                
                addSearch uid ["Spiderman", "Venom"] c 
                rs <- query c "SELECT id_t_user_account, search FROM comicspreviews.t_user_search WHERE id_t_user_account = ? " (Only uid) :: IO [(Int64, String)]
                rs `shouldBe` [(uid,"Spiderman"), (uid,"Venom")]
