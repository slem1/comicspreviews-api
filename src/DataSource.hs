module DataSource (
  openConnection    
) where

import qualified Data.Text as T
import qualified Data.Configurator as DC
import Data.Configurator.Types as DC_T
import PropertyUtil
import Database.PostgreSQL.Simple
    

openConnection :: IO Connection
openConnection = DC.load [DC.Required "application.properties"] >>= getConnectionInfo >>= connect 


getConnectionInfo :: DC_T.Config -> IO ConnectInfo
getConnectionInfo config = do  
    key <- DC.require config . T.pack $ "key_path"   
    host <- DC.require config . T.pack $ "db_host"
    port <- DC.require config . T.pack $ "db_port"
    username <- DC.require config . T.pack $ "db_username"
    password <- getEncryptedProperty config (T.pack "db_password") key         
    database <- DC.require config . T.pack $ "db_database"
    return $ ConnectInfo host port username password database
