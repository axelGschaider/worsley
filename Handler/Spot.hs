module Handler.Spot where

import Import
import Yesod.Auth
import Data.List (head)

maxAnswerPoints = 10
wrongAnswerPenalty = -4

data AnswerSelection = First | Second | Third | Fourth deriving (Show, Eq, Enum, Bounded)
data Sel = Sel AnswerSelection

getSpotR :: SpotId -> Handler Html
getSpotR spotId = do
  userId  <- requireAuthId
  spot    <- runDB $ get404 spotId
  props   <- runDB $ selectList [ PropositionSpot ==. spotId ][Asc PropositionOrd]
  answers <- runDB $ selectList [AnswerSpot ==. spotId][]
  yourAnswers <- return $ filter ( (== userId) . answerUser ) $ fmap clean answers
  itIsYours <- return $ userId == ( spotGroup spot )
  (answerWidget, enctype) <- generateFormPost selectionForm
  defaultLayout $ do
    setTitle $ toHtml $ spotTitle spot
    $(widgetFile "spot")



answers :: [(Text, AnswerSelection)]
answers =  [ ("1", First)
           , ("2", Second)
           , ("3",Third)
           , ("4", Fourth)
           ]

selectionForm :: Form Sel
selectionForm = renderDivs $ Sel <$> areq (selectFieldList answers) "Antwort" Nothing

toNumber :: AnswerSelection -> Int
toNumber First = 1
toNumber Second = 2
toNumber Third = 3
toNumber Fourth = 4

clean :: Entity a -> a
clean (Entity _ x) = x 

postSpotR :: SpotId -> Handler Html
postSpotR spotId = do
  groupId <- requireAuthId
  spot    <- runDB $ get404 spotId
  props   <- runDB $ selectList [ PropositionSpot ==. spotId ][Asc PropositionOrd]
  answers <- runDB $ selectList [AnswerSpot ==. spotId][]
  let plainAnswers = fmap clean answers
  let pointsToBeWon = computePoints plainAnswers
  let plainProps =  fmap clean props
  let correctProps = filter propositionCorrect plainProps
  let correctSelectionNumber = propositionOrd $ head correctProps
  ((res, answerWidget),enctype) <- runFormPost selectionForm
  case res of
    FormSuccess (Sel selection) -> do
          let seleNum = toNumber selection
          let correct = seleNum == correctSelectionNumber
          let points = if correct then pointsToBeWon else wrongAnswerPenalty
          _ <- runDB $ insert $ Answer spotId groupId correct points
          let correctAnswerCount = length $ filter answerCorrect plainAnswers
          let newCorretCount = if correct then correctAnswerCount + 1 else correctAnswerCount
          let newOwnersPoints = pointsForCreator newCorretCount
          _ <- runDB $ update spotId [SpotPointsForOwner =. newOwnersPoints, SpotCorrectAnswers =. newCorretCount]
          redirect $ SpotR spotId
    _ ->  error "da ist was schief gelaufen. Blöd."




computePoints :: [Answer] -> Int
computePoints as = base + additional
  where numberOfPositive = length $ filter answerCorrect as
        xxx = numberOfPositive + 1
        base = div maxAnswerPoints xxx
        additional = case (rem maxAnswerPoints xxx) of
                        0 -> 0
                        _ -> 1

pointsForCreator 0 = -4
pointsForCreator 1 = 7
pointsForCreator 2 = 12
pointsForCreator 3 = 6
pointsForCreator 4 = 3
pointsForCreator _ = 1

