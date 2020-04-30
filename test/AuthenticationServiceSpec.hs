module AuthenticationServiceSpec where

import Data.Text as T
import qualified Data.Configurator as DC
import Data.Configurator.Types as DC_T
import Control.Monad.Trans.Reader
import Test.Hspec
import Test.HUnit


spec :: Spec
spec = describe "Tests for AuthenticationService module" $ do    

     it "should 0 equals to 0" $ do        
        0 `shouldBe` 0      
