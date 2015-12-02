#!/usr/bin/env runhaskell
-- includes.hs

module Text.Pandoc.Include where

import Control.Monad
import Data.List
import System.Directory

import Text.Pandoc.JSON
import Text.Pandoc
import Text.Pandoc.Error


stripPandoc :: Either PandocError Pandoc -> [Block]
stripPandoc p =
  case p of
    Left _ -> [Null]
    Right (Pandoc _ blocks) -> blocks

ioReadMarkdown :: String -> IO(Either PandocError Pandoc)
ioReadMarkdown content = return $! readMarkdown def content

getContent :: String -> IO [Block]
getContent file = do
  c <- readFile file
  p <- ioReadMarkdown c
  return $! stripPandoc p

getProcessableFileList :: String -> IO [String]
getProcessableFileList list = do
  let f = lines list
  let files = filter (\x -> not $ "#" `isPrefixOf` x) f
  filterM doesFileExist files

processFiles :: [String] -> IO [Block]
processFiles toProcess =
  fmap concat (mapM getContent toProcess)

doInclude :: Block -> IO [Block]
doInclude (CodeBlock (_, classes, _) list)
  | "include" `elem` classes = do
    let toProcess = getProcessableFileList list
    processFiles =<< toProcess
doInclude x = return [x]

main :: IO ()
main = toJSONFilter doInclude
