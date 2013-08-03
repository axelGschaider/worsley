module Handler.AddUser where

import Import
import Data.Digest.Pure.MD5
import Data.Digest.Pure.SHA        (sha1, showDigest)
import qualified Data.ByteString.Lazy.Char8 as BS (pack)
import Data.Text                   (Text, pack, unpack, append)


getAddUserR :: Handler Html
getAddUserR = do
  (formWidget,enctype) <- generateFormPost userForm
  defaultLayout $ do
    $(widgetFile "adder")


postAddUserR :: Handler Html
postAddUserR = do
  ((res, userWidget),enctype) <- runFormPost userForm
  case res of
    FormSuccess user -> do
      _ <- runDB $ insert $ simpleUser2Group user
      redirect UsersR
    _ -> error "alles schief gelaufen!"



data SimpleUser = SimpleUser { name     ::Text
                             , password ::Text}

salt = "123"

simpleUser2Group :: SimpleUser -> WGroup
simpleUser2Group x = WGroup gname gpassword salt plainpassword
  where gname = name x
        plainpassword = password x
        gpassword = computePasswordHash plainpassword

computePasswordHash = saltedHash salt
--computePasswordHash :: Text -> Text
--computePasswordHash = T.pack . computePasswordHash'

--computePasswordHash' :: Text -> String
--computePasswordHash' plainPassword = show (md5 $ fromString $ (show plainPassword) )
----computePasswordHash' plainPassword = salt ++ show (md5 $ fromString $ salt ++ (show plainPassword) )


userForm :: Form SimpleUser
userForm = renderDivs $ SimpleUser
    <$> areq textField "Name" Nothing
    <*> areq textField "Passwort" Nothing



--copied
-- | Calculate salted hash using SHA1.
saltedHash :: Text              -- ^ Salt
           -> Text              -- ^ Password
           -> Text
saltedHash salt = 
  pack . showDigest . sha1 . BS.pack . unpack . append salt