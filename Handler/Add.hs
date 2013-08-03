module Handler.Add where

import Import
import System.Random
import qualified Data.Map as M
import qualified Data.List as L
import Yesod.Auth
import Yesod.Form.Nic (YesodNic, nicHtmlField)

instance YesodNic App

newSpotCost = -4
basePoints = 19::Int

data FrontEndSpot = FrontEndSpot { title::Text
                                 , description::Html
                                 , correctAnswer::Text
                                 , wrongAnswer1::Text  
                                 , wrongAnswer2::Text  
                                 , wrongAnswer3::Text  
                                 }

getAddR :: Handler RepHtml
getAddR = do 
    _ <- requireAuth
    (formWidget, enctype) <- generateFormPost spotForm
    defaultLayout $ do
      $(widgetFile "adder")

postAddR :: Handler RepHtml
postAddR = do
    userId <- requireAuthId
    ((res, formWidget),enctype) <- runFormPost spotForm
    case res of
      FormSuccess spot ->  do
          spotId    <-  runDB $ insert $ frontEnd2BackEndSpot spot userId
          _ <-  mapM (runDB . insert) $ frontEnd2Props spot spotId
          defaultLayout $ do
            redirect $ SpotR spotId
      _ -> error "er ist tot, Jim"

spotForm :: Form FrontEndSpot
spotForm = renderDivs $ FrontEndSpot
    <$> areq textField "Titel" Nothing
    <*> areq nicHtmlField "Beschreibung" Nothing
    <*> areq textField "richtige Antwort" Nothing
    <*> areq textField "falsche Antwort 1" Nothing
    <*> areq textField "falsche Antwort 2" Nothing
    <*> areq textField "falsche Antwort 3" Nothing

frontEnd2Props :: FrontEndSpot -> SpotId -> [Proposition]
frontEnd2Props spot sid = daList
    where correct = (correctAnswer spot, True)
          wrong1 = (wrongAnswer1 spot, False)
          wrong2 = (wrongAnswer2 spot, False)
          wrong3 = (wrongAnswer3 spot, False)
          unshuffled = [correct, wrong1, wrong2, wrong3]
          shuffledAndIndexed = zip [1 ..] $ shuffle unshuffled
          daList = map (toProp sid) shuffledAndIndexed

toProp :: SpotId -> (Int, (Text,Bool)) -> Proposition
toProp i (order, (text, corr)) = Proposition i text corr order

shuffle :: [(Text, Bool)] -> [(Text, Bool)]
shuffle a = fst $ fisherYates gen a
  where xxx = sum $ map ( length . show . fst ) a
        gen = mkStdGen xxx

frontEnd2BackEndSpot :: FrontEndSpot -> WGroupId -> Spot
frontEnd2BackEndSpot s g = Spot t d g newSpotCost 0
                        where t = title s
                              d = description s

 
fisherYatesStep :: RandomGen g => (M.Map Int a, g) -> (Int, a) -> (M.Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((M.insert j x . M.insert i (m M.! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen
 
fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
  toElems $ foldl fisherYatesStep (initial (L.head l) gen) (numerate (L.tail l))
  where
    toElems (x, y) = (M.elems x, y)
    numerate = zip [1..]
    initial x gen = (M.singleton 0 x, gen)

clean :: Entity a -> a
clean (Entity _ x) = x 
