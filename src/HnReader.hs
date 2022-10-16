-- |
-- Copyright: (c) 2021 Lucian Ursu
-- SPDX-License-Identifier: MIT
-- Maintainer: Lucian Ursu <lucian.ursu@gmail.com>
--
-- It allows you to read Hacker News without a browser.
module HnReader
  ( getTopStories,
    getNewStories,
    getComments,
  )
where

import Control.Lens ((^.))
import Data.Aeson (FromJSON (..), ToJSON (..), genericParseJSON, genericToEncoding)
import qualified Data.Text as T
import qualified Data.Text.Internal.Builder as TB
import qualified Data.Text.Lazy as TL
import GHC.Generics ()
import HTMLEntities.Decoder (htmlEncodedText)
import JsonUtils (encodingOptions)
import Network.URI (URI (uriAuthority), URIAuth (uriRegName), parseURI)
import qualified Network.Wreq as Wreq
import Text.Display (Display (..), dShow, mkDt)

getTopStories :: Int -> Int -> IO [Text]
getTopStories storiesPerPage pageIndex = do
  items <- getItems topStoriesURL storiesPerPage pageIndex
  pure $ map (render . itemToStory) items

getNewStories :: Int -> Int -> IO [Text]
getNewStories storiesPerPage pageIndex = do
  items <- getItems newStoriesURL storiesPerPage pageIndex
  pure $ map (render . itemToStory) items

getComments :: Int -> Int -> Int -> IO [Text]
getComments storyId commentsPerPage pageIndex = do
  item <- getItem storyId
  let commentIds = fromMaybe [] $ iKids item
      commentIdsToDisplay = pageItems commentsPerPage pageIndex commentIds
  items <- mapM getItem commentIdsToDisplay
  let cleanItems = filter isAlive items
  pure $ map (render . itemToComment) cleanItems
  where
    -- It looks like, if the `deleted` field is missing, then the item is alive
    isAlive item = not $ fromMaybe False $ iDeleted item

render :: Display a => a -> Text
render = dShow . display

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

getItem :: Int -> IO Item
getItem id_ = do
  jsonResponse <- Wreq.get $ itemURL id_
  response <- Wreq.asJSON jsonResponse
  pure $ response ^. Wreq.responseBody

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

-- STORY
data StoryItem = StoryItem
  { siId :: Int,
    siBy :: Maybe Text,
    siTime :: Maybe Int,
    siUrl :: Maybe Text,
    siTitle :: Maybe Text,
    siScore :: Maybe Int,
    siDescendants :: Maybe Int
  }

instance Display StoryItem where
  display storyItem =
    let rawScore = fromMaybe 0 $ siScore storyItem
        score =
          if rawScore == 0
            then "N/A"
            else show rawScore
        title = fromMaybe "N/A" $ siTitle storyItem
        urlHost = getHost storyItem
        commentCount = show $ fromMaybe 0 $ siDescendants storyItem
        id_ = show $ siId storyItem
        separator = T.pack $ replicate (T.length $ score <> title) '-'
     in mkDt $
          separator <> "\n"
            <> id_
            <> " | "
            <> title
            <> " ("
            <> urlHost
            <> ")"
            <> "\n"
            <> score
            <> " points | "
            <> commentCount
            <> " comments | "

getHost :: StoryItem -> Text
getHost item =
  let maybeHost = do
        rawURI <- siUrl item
        uri <- parseURI $ T.unpack rawURI
        authority <- uriAuthority uri
        pure $ uriRegName authority
      host = T.pack $ fromMaybe "N/A" maybeHost
   in if T.isPrefixOf "www." host
        then T.drop 4 host
        else host

itemToStory :: Item -> StoryItem
itemToStory item =
  StoryItem
    { siId = iId item,
      siBy = iBy item,
      siTime = iTime item,
      siUrl = iUrl item,
      siTitle = iTitle item,
      siScore = iScore item,
      siDescendants = iDescendants item
    }

-- COMMENT
data CommentItem = CommentItem
  { ciId :: Int,
    ciBy :: Maybe Text,
    ciTime :: Maybe Int,
    ciText :: Maybe Text,
    ciKids :: Maybe [Int]
  }

itemToComment :: Item -> CommentItem
itemToComment item =
  CommentItem
    { ciId = iId item,
      ciBy = iBy item,
      ciTime = iTime item,
      ciText = clean <$> iText item,
      ciKids = iKids item
    }
  where
    clean text =
      let encoded = TL.toStrict $ TB.toLazyText $ htmlEncodedText text
       in T.replace "<p>" "\n\n" encoded

instance Display CommentItem where
  display commentItem =
    let id_ = show $ ciId commentItem
        by = fromMaybe "N/A" $ ciBy commentItem
        text = fromMaybe "N/A" $ ciText commentItem
        replies = show $ length $ fromMaybe [] $ ciKids commentItem
        separator = T.pack $ replicate 30 '-'
     in mkDt $
          separator <> "\n"
            <> id_
            <> " | "
            <> text
            <> "\n"
            <> "by: "
            <> by
            <> " | "
            <> replies
            <> " replies"

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