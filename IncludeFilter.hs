#!/usr/bin/env runhaskell
-- includes.hs

module Text.Pandoc.Include where

import Control.Monad
import Data.List.Split

import Text.Pandoc.JSON
import Text.Pandoc
import Text.Pandoc.Error


stripPandoc :: Either PandocError Pandoc -> [Block]
stripPandoc p =
  case p of
    Left _ -> [Null]
    Right (Pandoc _ blocks) -> blocks


ioReadMarkdown :: String -> IO(Either PandocError Pandoc)
ioReadMarkdown content = return (readMarkdown def content)


getContent :: String -> IO [Block]
getContent file = do
  c <- readFile file
  p <- ioReadMarkdown c
  return (stripPandoc p)


doInclude :: Block -> IO [Block]
doInclude cb@(CodeBlock (_, classes, _) list) =
  if "include" `elem` classes
    then do
      -- msum $ map getContent (splitOn "\n" list)
      files <- return $ wordsBy (=='\n') list
      contents <- return $ map getContent files
      result <- return $ msum contents
      result
      -- msum $ map (\file ->
      --                 do
      --                   c <- readFile file
      --                   p <- ioReadMarkdown c
      --                   return (stripPandoc p)
      --             ) (wordsBy (=='\n') list)
    else
        return [cb]
doInclude x = return [x]


main :: IO ()
main = toJSONFilter doInclude
