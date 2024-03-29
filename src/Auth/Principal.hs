module Auth.Principal
  ( Principal(..)
  , hashPassword
  )
where

import           Data.ByteString
import           Crypto.Hash
import           Data.Int

data Principal = Principal {
    uid :: Int64,
    username :: String,
    enabled :: Bool,
    password :: String
} deriving (Eq)

instance Show Principal where
  show (Principal i u e _) = mconcat [show u, ":",show i, "--", show e]

hashPassword :: ByteString -> String
hashPassword pass = show (hashWith SHA256 pass) :: String
