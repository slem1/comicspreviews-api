module UserAccountService (    

) where

import UserAccount
import Crypto.Hash

create :: UserAccount -> String -> IO UserAccount
create acc rawPassword = "INSERT INTO comicspreviews.user_account ('username', 'email')"
    where
        password = hashWith SHA256 password


