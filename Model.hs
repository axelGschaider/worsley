module Model where

import Prelude
import Yesod
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import Yesod.Auth.HashDB (HashDBUser, userPasswordHash, userPasswordSalt, setSaltAndPasswordHash)

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

-- in Model.hs
instance HashDBUser WGroup where
    userPasswordHash = Just . wGroupPassword
    userPasswordSalt = Just . wGroupSalt
    setSaltAndPasswordHash s h p = p { wGroupSalt     = s
                                     , wGroupPassword = h
                                     }