-- |
-- Copyright: (c) 2021 Lucian Ursu
-- SPDX-License-Identifier: MIT
-- Maintainer: Lucian Ursu <lucian.ursu@gmail.com>
--
-- It allows you to read Hacker News without a browser.
module HnReader
  ( getTopStories,
    getNewStories,
  )
where

import Control.Lens ((^.))
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding)
import GHC.Generics ()
import JsonUtils (encodingOptions)
import qualified Network.Wreq as Wreq
import Text.Display (Display (..), dShow, mkDtStr)

getTopStories :: Int -> Int -> IO [Text]
getTopStories storiesPerPage pageIndex = do
  stories <- getItems topStoriesURL storiesPerPage pageIndex
  pure $ map (dShow . display) stories

-- getTopStories :: IO [Story]

getNewStories :: Int -> Int -> IO [Text]
getNewStories storiesPerPage pageIndex = do
  stories <- getItems newStoriesURL storiesPerPage pageIndex
  pure $ map (dShow . display) stories

getItems :: [Char] -> Int -> Int -> IO [Item]
getItems itemsURL itemsPerPage pageIndex = do
  jsonResponse <- Wreq.get itemsURL
  response <- Wreq.asJSON jsonResponse -- this function should really be named `decodeJSON`
  let itemsIds = response ^. Wreq.responseBody
      itemsIdsToDisplay = pageItems itemsPerPage pageIndex itemsIds
  mapM getItem itemsIdsToDisplay

pageItems :: Int -> Int -> [a] -> [a]
pageItems itemsPerPage pageIndex items =
  take (pageIndex * itemsPerPage) $ drop ((pageIndex - 1) * itemsPerPage) items

-- getStory :: Int -> IO Story
-- getStory id_ = undefined

-- getJobs :: IO [Job]

-- getPolls :: IO [Poll]

-- getPollOpts :: IO [PollOpt]

-- getComments :: Int -> IO [Comment]
-- getComments storyId = undefined

-- getCommentThread :: Int -> IO [Comment]
-- getCommendThread threadParentId = undefined

-- getUser :: Text -> IO User
-- getUser username = undefined

getItem :: Int -> IO Item
getItem id_ = do
  jsonResponse <- Wreq.get $ itemURL id_
  response <- Wreq.asJSON jsonResponse
  pure $ response ^. Wreq.responseBody

-- ITEMS
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
    iUrl :: Maybe Text,
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

-- USERS
data User = User
  { uId :: Text,
    uKarma :: Int,
    uCreated :: Int,
    uAbout :: Maybe Text,
    uSubmitted :: [Int]
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

newStoriesURL :: [Char]
newStoriesURL = apiBaseURL <> "newstories.json"

itemURL :: Int -> [Char]
itemURL id_ =
  apiBaseURL <> "item/" <> show id_ <> ".json"

userURL :: Text -> [Char]
userURL user =
  apiBaseURL <> "user/" <> show user <> ".json"