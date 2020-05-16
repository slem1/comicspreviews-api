module SpecUtils( 
    withDatabaseConnection,
    withTransactionRollback
) where

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

openConnection :: IO Connection
openConnection = DC.load [DC.Required "test/misc/test.properties"] >>= getConnectionInfo >>= connect 

closeConnection :: Connection -> IO ()
closeConnection = close  

withDatabaseConnection :: (Connection -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection closeConnection

withTransactionRollback :: (Connection -> IO n) -> Connection -> IO n
withTransactionRollback op c = withTransaction c $ do        
    r <- op c
    rollback c
    return r

getConnectionInfo :: DC_T.Config -> IO ConnectInfo
getConnectionInfo config = do  
    key <- DC.require config . T.pack $ "key_path"   
    host <- DC.require config . T.pack $ "db_host"
    port <- DC.require config . T.pack $ "db_port"
    username <- DC.require config . T.pack $ "db_username"
    password <- getEncryptedProperty config (T.pack "db_password") key         
    database <- DC.require config . T.pack $ "db_database"
    return $ ConnectInfo host port username password database