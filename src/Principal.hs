module Principal (
    Principal(..),
    hashPassword
) where

import Data.ByteString
import Crypto.Hash

data Principal = Principal {
    username :: String,    
    enabled :: Bool,
    password :: String
} deriving (Eq)

instance Show Principal where
    show (Principal u e _) = mconcat [show u, "--", show e]

hashPassword :: ByteString -> String
hashPassword pass = show (hashWith SHA256 pass) :: String        