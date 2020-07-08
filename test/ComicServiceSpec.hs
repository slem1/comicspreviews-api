{-# LANGUAGE OverloadedStrings #-}
module ComicServiceSpec where

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


import Data.Time.Calendar

import Service.ComicService
import qualified Model.UserAccount as UA
import PropertyUtil
import Model.Comic

testAccount = UA.UserAccount { UA.id = -1, UA.username = "wolverine", UA.email = "wolverine@krakoa.com", UA.enabled = False}

spec :: Spec
spec =  
    around withDatabaseConnection $      
        describe "Tests for ComicService module" $ do 
           it "should find user comics" $ withTransactionRollback $ \c -> do           
                let date1 = fromGregorian 2020 4 3
                let date2 = fromGregorian 2020 4 10
                let catalogParam1 = (date1, "") :: (Day, String)             
                let catalogParam2 = (date2, "") :: (Day, String)             
                [Only cid1]<- query c "INSERT INTO comicspreviews.t_catalog (date_release, filepath) VALUES (?, ?) RETURNING id_t_catalog" catalogParam1
                [Only cid2]<- query c "INSERT INTO comicspreviews.t_catalog (date_release, filepath) VALUES (?, ?) RETURNING id_t_catalog" catalogParam2
                let comicsDataSet = 
                        [(1,cid1, "JAN2025", "SPIDERMAN", "$4", "MARVEL COMICS"),
                        (2, cid1, "JAN2026", "X-MEN DX #1 Var", "$4", "MARVEL COMICS"),
                        (3, cid1, "JAN2027", "BATMAN", "$4", "DC COMICS"),
                        (4, cid2, "JAN2028", "BLACK CAT EXTRA #9", "$4", "DC COMICS")] :: [(Int64,Int64, String, String, String, String)]                
                executeMany c "INSERT INTO comicspreviews.t_comic(id_t_comic,id_t_catalog, reference, title, price, editor) VALUES (?,?, ?, ?, ?, ?)" comicsDataSet                    
                [Only uid] <- query c "INSERT INTO comicspreviews.t_user_account (username, email, enabled, password) VALUES (?, ?, ?, ?) RETURNING id_t_user_account" (UA.username testAccount, UA.email testAccount, True, "password" :: String) :: IO [Only Int64]                
                execute c "INSERT INTO comicspreviews.t_user_search (id_t_user_account, search) VALUES (?, 'BLACK CAT')" (Only uid)
                execute c "INSERT INTO comicspreviews.t_user_search (id_t_user_account, search) VALUES (?, 'x-men')" (Only uid)
                rs <- getUserComics uid date1 Nothing c    
                rs `shouldBe` 
                    [Comic 2 "JAN2026" "X-MEN DX #1 Var" "" "MARVEL COMICS" (fromGregorian 2020 4 3),
                     Comic 4 "JAN2028" "BLACK CAT EXTRA #9" "" "DC COMICS" (fromGregorian 2020 4 10)]   

           it "should find no comics" $ withTransactionRollback $ \c -> do           
                let date = fromGregorian 2020 4 3
                let catalogParam = (date, "") :: (Day, String)             
                [Only cid]<- query c "INSERT INTO comicspreviews.t_catalog (date_release, filepath) VALUES (?, ?) RETURNING id_t_catalog" catalogParam
                let comicsDataSet = 
                            [(1,cid, "JAN2025", "SPIDERMAN", "$4", "MARVEL COMICS"),
                            (2, cid, "JAN2026", "X-MEN DX #1 Var", "$4", "MARVEL COMICS"),
                            (3, cid, "JAN2027", "BATMAN", "$4", "DC COMICS"),
                            (4, cid, "JAN2028", "BLACK CAT EXTRA #9", "$4", "DC COMICS")] :: [(Int64,Int64, String, String, String, String)]                
                executeMany c "INSERT INTO comicspreviews.t_comic(id_t_comic,id_t_catalog, reference, title, price, editor) VALUES (?,?, ?, ?, ?, ?)" comicsDataSet                    
                [Only uid] <- query c "INSERT INTO comicspreviews.t_user_account (username, email, enabled, password) VALUES (?, ?, ?, ?) RETURNING id_t_user_account" (UA.username testAccount, UA.email testAccount, True, "password" :: String) :: IO [Only Int64]                                
                rs <- getUserComics uid date Nothing c    
                rs `shouldBe` []              

           it "should find user comics in boundary" $ withTransactionRollback $ \c -> do           
                let date1 = fromGregorian 2020 4 3
                let date2 = fromGregorian 2020 4 10
                let catalogParam1 = (date1, "") :: (Day, String)             
                let catalogParam2 = (date2, "") :: (Day, String)             
                [Only cid1]<- query c "INSERT INTO comicspreviews.t_catalog (date_release, filepath) VALUES (?, ?) RETURNING id_t_catalog" catalogParam1
                [Only cid2]<- query c "INSERT INTO comicspreviews.t_catalog (date_release, filepath) VALUES (?, ?) RETURNING id_t_catalog" catalogParam2
                let comicsDataSet = 
                        [(1,cid1, "JAN2025", "SPIDERMAN", "$4", "MARVEL COMICS"),
                        (2, cid1, "JAN2026", "X-MEN DX #1 Var", "$4", "MARVEL COMICS"),
                        (3, cid1, "JAN2027", "BATMAN", "$4", "DC COMICS"),
                        (4, cid2, "JAN2028", "BLACK CAT EXTRA #9", "$4", "DC COMICS")] :: [(Int64,Int64, String, String, String, String)]                
                executeMany c "INSERT INTO comicspreviews.t_comic(id_t_comic,id_t_catalog, reference, title, price, editor) VALUES (?,?, ?, ?, ?, ?)" comicsDataSet                    
                [Only uid] <- query c "INSERT INTO comicspreviews.t_user_account (username, email, enabled, password) VALUES (?, ?, ?, ?) RETURNING id_t_user_account" (UA.username testAccount, UA.email testAccount, True, "password" :: String) :: IO [Only Int64]                
                execute c "INSERT INTO comicspreviews.t_user_search (id_t_user_account, search) VALUES (?, 'BLACK CAT')" (Only uid)
                execute c "INSERT INTO comicspreviews.t_user_search (id_t_user_account, search) VALUES (?, 'x-men')" (Only uid)
                rs <- getUserComics uid date1 (Just date1) c    
                rs `shouldBe` [ Comic 2 "JAN2026" "X-MEN DX #1 Var" "" "MARVEL COMICS" (fromGregorian 2020 4 3)]         

           it "should find comics with pagination" $ withTransactionRollback $ \c -> do           
                let date1 = fromGregorian 2020 4 3                
                let catalogParam1 = (date1, "") :: (Day, String)                             
                [Only cid1]<- query c "INSERT INTO comicspreviews.t_catalog (date_release, filepath) VALUES (?, ?) RETURNING id_t_catalog" catalogParam1                
                let comicsDataSet = 
                        [(1,cid1, "JAN2025", "SPIDERMAN", "$4", "MARVEL COMICS"),
                        (2, cid1, "JAN2026", "X-MEN DX #1 Var", "$4", "MARVEL COMICS"),
                        (3, cid1, "JAN2027", "BATMAN", "$4", "DC COMICS"),
                        (4, cid1, "JAN2028", "BLACK CAT EXTRA #9", "$4", "DC COMICS"),
                        (5, cid1, "JAN2029", "VENOM", "$4", "MARVEL COMICS") ] :: [(Int64,Int64, String, String, String, String)]                                
                executeMany c "INSERT INTO comicspreviews.t_comic(id_t_comic,id_t_catalog, reference, title, price, editor) VALUES (?,?, ?, ?, ?, ?)" comicsDataSet                    
                [Only uid] <- query c "INSERT INTO comicspreviews.t_user_account (username, email, enabled, password) VALUES (?, ?, ?, ?) RETURNING id_t_user_account" (UA.username testAccount, UA.email testAccount, True, "password" :: String) :: IO [Only Int64]                                
                let uidTermEntries = [ (uid, term) | term <- ["SPIDERMAN", "BATMAN", "BLACK CAT", "X-MEN", "VENOM"]] :: [(Int64, String)]                       
                executeMany c "INSERT INTO comicspreviews.t_user_search (id_t_user_account, search) VALUES (?, ?)" uidTermEntries                
                rs <- getUserComicsPaginated uid date1 Nothing (0, 2) c    
                rs `shouldBe` [ 
                        Comic 1 "JAN2025" "SPIDERMAN" "" "MARVEL COMICS" (fromGregorian 2020 4 3),
                        Comic 2 "JAN2026" "X-MEN DX #1 Var" "" "MARVEL COMICS" (fromGregorian 2020 4 3)]         
                rs <- getUserComicsPaginated uid date1 Nothing (2, 2) c    
                rs `shouldBe` [ 
                        Comic 3 "JAN2027" "BATMAN" "" "DC COMICS" (fromGregorian 2020 4 3),
                        Comic 4 "JAN2028" "BLACK CAT EXTRA #9" "" "DC COMICS" (fromGregorian 2020 4 3)]         
                rs <- getUserComicsPaginated uid date1 Nothing (4, 2) c    
                rs `shouldBe` [ 
                        Comic 5 "JAN2029" "VENOM" "" "MARVEL COMICS" (fromGregorian 2020 4 3)]         


