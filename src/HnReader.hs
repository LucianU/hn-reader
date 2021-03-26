-- |
-- Copyright: (c) 2021 Lucian Ursu
-- SPDX-License-Identifier: MIT
-- Maintainer: Lucian Ursu <lucian.ursu@gmail.com>
--
-- It allows you to read Hacker News without a browser.
module HnReader
  ( someFunc
  , getTopStories
  )
where

import Control.Lens ((^.))
import Data.Aeson (FromJSON(..), ToJSON(..), genericToEncoding, genericParseJSON)
import GHC.Generics ()
import qualified Network.Wreq as Wreq
import Text.Display (Display (..), mkDtStr, dShow)
import JsonUtils (encodingOptions)

someFunc :: IO ()
someFunc = do
  stories <- getTopStories
  mapM_ putStrLn stories

getTopStories :: IO [Text]
getTopStories = do
  r <- Wreq.asJSON =<< Wreq.get topStoriesURL
  let storyIds = r ^. Wreq.responseBody
  stories <- mapM getStory (take 30 storyIds)
  pure $ map (dShow . display) stories

-- getTopStories :: IO [Story]

-- getNewStories :: IO [Story]

-- getJobs :: IO [Job]

-- getPolls :: IO [Poll]

-- getPollOpts :: IO [PollOpt]

-- getComments :: IO [Comment]

-- getUser :: Text -> IO User
-- getUser username = undefined

getStory :: Int -> IO Item
getStory id_ = do
  r <- Wreq.get $ itemURL id_
  decodedResp <- Wreq.asJSON r
  pure $ decodedResp ^. Wreq.responseBody

data Item = Item
  { iId :: Int,
    iDeleted :: Maybe Bool,
    iType :: Maybe ItemType,
    iBy :: Maybe Text,
    iTime :: Maybe Int,
    iText :: Maybe Text,
    iDead :: Maybe Bool,
    iParent :: Maybe Int,
    iPoll :: Maybe Int,
    iKids :: Maybe [Int],
    iURL :: Maybe Text,
    iScore :: Maybe Int,
    iTitle :: Maybe Text,
    iParts :: Maybe [Int],
    iDescendants :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON Item where
  toEncoding = genericToEncoding encodingOptions

instance FromJSON Item where
  parseJSON = genericParseJSON encodingOptions

instance Display Item where
  display item =
    mkDtStr $
      show (iScore item) <> " " <> show (iTitle item)

data ItemType
  = Job
  | Story
  | Comment
  | Poll
  | PollOpt
  deriving (Show, Generic)

instance ToJSON ItemType where
  toEncoding = genericToEncoding encodingOptions

instance FromJSON ItemType where
  parseJSON = genericParseJSON encodingOptions

data User = User
  { uId :: Text
  , uKarma :: Int
  , uCreated :: Int
  , uAbout :: Maybe Text
  , uSubmitted :: [Int]
  }
  deriving (Show, Generic)

instance ToJSON User where
  toEncoding = genericToEncoding encodingOptions

instance FromJSON User where
  parseJSON = genericParseJSON encodingOptions

-- URLs
apiBaseURL :: [Char]
apiBaseURL = "https://hacker-news.firebaseio.com/v0/"

topStoriesURL :: [Char]
topStoriesURL = apiBaseURL <> "topstories.json"

itemURL :: Int -> [Char]
itemURL id_ =
  apiBaseURL <> "item/" <> show id_ <> ".json"

userURL :: Text -> [Char]
userURL user =
  apiBaseURL <> "user/" <> show user <> ".json"