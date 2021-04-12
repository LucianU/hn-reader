module Main (main) where

import HnReader (getNewStories, getTopStories)
import Options.Applicative (Parser, auto, command, execParser, help, idm, info, long, metavar, option, progDesc, short, showDefault, subparser, value)
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

printStories :: (Int -> Int -> IO [Text]) -> Int -> Int -> IO ()
printStories getStories storiesPerPage pageIndex = do
  stories <- getStories storiesPerPage pageIndex
  putStrLn $ unlines stories