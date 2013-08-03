module Handler.Worsley where

import Import
import Yesod.Auth

basePoints = 14::Int

getWorsleyR :: Handler Html
getWorsleyR = do
  user <- requireAuth
  userId <- requireAuthId
  plainUser <- return $ clean user
  spots <- runDB $ selectList [] [Desc SpotId]
  harvesterPoints <- return $ sum $ map spotPointsForOwner $ filter ( (== userId) . spotGroup ) $ map clean spots
  myAnswers <- runDB $ selectList [AnswerUser ==. userId][]
  hunterPoints <- return $ sum $ map (answerPoints . clean) myAnswers
  baseP <- return basePoints
  points <- return $ harvesterPoints + hunterPoints + basePoints
  canCreate <- return $ points >= 5
  defaultLayout $ do
    $(widgetFile "spots")
    
clean :: Entity a -> a
clean (Entity _ x) = x 
