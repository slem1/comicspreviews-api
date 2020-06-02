module PropertyUtil
  ( getEncryptedProperty
  )
where

import           Crypto
import           Data.Configurator
import           Data.Configurator.Types
import           Data.Text

getEncryptedProperty :: Config -> Text -> FilePath -> IO String
getEncryptedProperty config prop keyPath = do
  encrypted  <- require config prop
  eDecrypted <- decryptProperty encrypted keyPath
  case eDecrypted of
    Left  e -> error e
    Right r -> return r

-- |The 'decryptProperty' decrypts a potentially AES256 encrypted property. If the value the raw value of the
--  property, it should be prefix with {CLEAR}
decryptProperty
  :: String                          -- ^ The 'property value' argument
  -> FilePath                     -- ^ The 'path' of the private key file
  -> IO (Either String String)    -- ^ The return property value
decryptProperty ('{' : 'C' : 'L' : 'E' : 'A' : 'R' : '}' : xs) _ =
  return $ Right xs
decryptProperty xs keyFile = decryptString xs keyFile
