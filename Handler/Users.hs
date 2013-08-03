module Handler.Users where

import Import

getUsersR :: Handler Html
getUsersR = do
  users <- runDB $ selectList [] [Desc WGroupId]
  defaultLayout $ do
    $(widgetFile "users")

postUsersR :: Handler Html
postUsersR = error "Not yet implemented: postUsersR"

