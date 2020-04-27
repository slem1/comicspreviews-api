{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}

module Crypto (
    generateKey,        
    encryptString,
    decryptString
)
where

import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV, KeySizeSpecifier(..), IV, makeIV)
import Crypto.Error (CryptoFailable(..), CryptoError(..))

import qualified Crypto.Random.Types as CRT

import Data.ByteArray (ByteArray)
import Data.ByteString (ByteString)

import qualified Data.ByteString as BS 
import System.IO
import Data.ByteString.UTF8
import qualified Data.ByteString.Base64 as B64

initIV :: forall m. (CRT.MonadRandom m) => ByteString -> m (Maybe (IV AES256))
initIV v = return $ makeIV v    

-- |The 'generatePrivateKey' create a private key file based on AES256 with an initialization vector of 16 bytes preprend to the 32 bytes-length key
generateKey :: 
  FilePath    -- ^ The 'path' of the output key file
  -> IO ()    -- ^ The 'returned IO' argument
generateKey path = do 
  iv :: ByteString <- CRT.getRandomBytes $ blockSize (undefined :: AES256)
  bytes <- CRT.getRandomBytes 32   
  h <- openFile path ReadWriteMode
  BS.hPut h iv
  BS.hPut h bytes   
  hClose h

-- |The 'readKey' read a private key file and return the ( iv, k) parts
readKey :: FilePath              -- ^ The 'path' of the input key file
  -> IO (ByteString, ByteString) -- ^ The 'return IO of (iv,k)' where iv is the initialization vector and k the key part
readKey path = do 
  h <- openFile path ReadMode
  iv <- BS.hGet h 16
  key <- BS.hGet h 32
  hClose h
  return (iv, key) 

-- |The 'encryptString' encrypt an UTF-8 String with AES256. It returns a Base64 encoded encrypted String.
encryptString :: 
  String            -- ^ The 'raw String' argument 
  -> FilePath       -- ^ The 'path' of the private key file 
  -> IO String      -- ^ The return String encoded in Base64.
encryptString msg keyFile = toString <$> encryptMsg msg keyFile B64.encode 

-- |The 'decryptString' decrypt an UTF-8 encoded Base64 String with AES256. It returns the UTF-8 encoded raw value.
decryptString :: 
  String                        -- ^ The 'b64 String' argument 
  -> FilePath                   -- ^ The 'path' of the private key file 
  -> IO (Either String String)  -- ^ The return value
decryptString msg keyFile = do 
  eraw <- decryptMsg msg keyFile B64.decode
  return $ case eraw of
    Left e -> Left e 
    Right raw -> Right $ toString raw 


-- |The 'encrypt' encrypts a message   
encrypt :: (BlockCipher c) => 
  IV c              -- ^ The 'initialization vector' argument
  -> ByteString     -- ^ The 'key' argument
  -> ByteString     -- ^ The 'message' argument
  -> ByteString     -- ^ The 'encrypted result'
encrypt iv key msg = case (cipherInit key) of
  CryptoFailed err -> error $ show err
  CryptoPassed c -> ctrCombine c iv msg


decrypt :: (BlockCipher c) => 
  IV c            -- ^ The 'initialization vector' argument
  -> ByteString   -- ^ The 'key' argument
  -> ByteString   -- ^ The 'message' argument
  -> ByteString   -- ^ The 'decrypted result'
decrypt = encrypt  
  
encryptMsg :: String -> FilePath -> (ByteString -> ByteString) -> IO ByteString
encryptMsg msg keyPath encoder = do
  (v, k) <- readKey keyPath
  mIV <- initIV v
  case mIV of
    Nothing -> error "Cannot create initialization vector"
    Just iv -> return $  encoder . encrypt iv k $ msg'      
  where
    msg'= fromString msg 

decryptMsg :: String -> FilePath -> (ByteString -> Either String ByteString) -> IO (Either String ByteString) 
decryptMsg msg keyPath decoder  = do
  (v, k) <- readKey keyPath
  mIV <- initIV v
  case mIV of
    Nothing -> error "Cannot create initialization vector"
    Just iv -> do 
      eMsg <- return $ decoder . fromString $ msg
      case eMsg of
        Left e -> return $ Left e
        Right msg' -> return $ Right $ decrypt iv k msg'     

     