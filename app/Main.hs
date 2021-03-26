module Main (main) where

import HnReader (getTopStories)

main :: IO ()
main = do
  stories <- getTopStories
  mapM_ putStrLn stories
