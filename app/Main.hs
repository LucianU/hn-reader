module Main (main) where

import HnReader (getComments, getNewStories, getTopStories)
import Options.Applicative (Parser, auto, command, execParser, help, idm, info, long, metavar, option, progDesc, short, showDefault, subparser, value, argument)
import Prelude hiding (option)

{- Interface
hn-reader top

hn-reader new

hn-reader story [id]

hn-reader comments [story-id]

hn-reader thread [parent-comment-id]

hn-reader ask

hn-reader show

hn-reader jobs
-}

main :: IO ()
main = join $ execParser (info opts idm)

opts :: Parser (IO ())
opts =
  subparser
    ( command "top" (info (printStories getTopStories <$> storiesPerPageOpt <*> pageOpt) (progDesc "Show top stories"))
        <> command "new" (info (printStories getNewStories <$> storiesPerPageOpt <*> pageOpt) (progDesc "Show new stories"))
        <> command "comments" (info (printComments <$> storiesPerPageOpt <*> pageOpt <*> storyIdArg) (progDesc "Show the comments on a story"))
    )
  where
    storiesPerPageOpt :: Parser Int
    storiesPerPageOpt =
      option
        auto
        ( long "stories-per-page"
            <> short 's'
            <> value 30
            <> showDefault
            <> metavar "INT"
            <> help "The number of stories per page"
        )

    pageOpt :: Parser Int
    pageOpt =
      option
        auto
        ( long "page"
            <> short 'p'
            <> value 1
            <> metavar "INT"
            <> help "The page we want to display"
        )
    storyIdArg :: Parser Int
    storyIdArg =
      argument
        auto
        (metavar "STORY-ID")

printStories :: (Int -> Int -> IO [Text]) -> Int -> Int -> IO ()
printStories getStories storiesPerPage pageIndex = do
  stories <- getStories storiesPerPage pageIndex
  putStrLn $ unlines stories

printComments :: Int -> Int -> Int -> IO ()
printComments itemsPerPage pageIndex storyId = do
  comments <- getComments storyId itemsPerPage pageIndex
  putStrLn $ unlines comments