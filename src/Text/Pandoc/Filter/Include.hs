{-# LANGUAGE OverloadedStrings #-}
module Text.Pandoc.Filter.Include where

{-|
A Pandoc filter that replaces include labeled Code Blocks with the contents of
the referenced files. Even nested, recursive includes.

Based on the scripting tutorial for Pandoc:
http://pandoc.org/scripting.html#include-files

The Code Blocks like the following will include every file in a new line. The
reference paths should be either absolute or relative to the folder where the
pandoc command will be executed.

> ```include
> /absolute/file/path.md
> relative/to/the/command/root.md
> #do/not/include/this.md
> ```

If the file does not exist, it will be skipped completely. No warnings, no
residue, nothing. Putting an # as the first character in the line will make the
filter skip that file.

For now the nested includes only work for two levels, after that the source
will be inserted and not parsed.

Note: the metadata from the included source files are discarded.

-}

import           Control.Monad
import           Data.List
import           System.Directory

import           Data.Text (Text)
import qualified Data.Text as Text

import           Text.Pandoc

stripPandoc :: Either PandocError Pandoc -> [Block]
stripPandoc p =
  case p of
    Left _ -> [Null]
    Right (Pandoc _ blocks) -> blocks

ioReadMarkdown :: Text -> IO (Either PandocError Pandoc)
ioReadMarkdown content = runIO $ readMarkdown def content

getContent :: String -> IO [Block]
getContent file = do
  c <- readFile file
  p <- ioReadMarkdown $ Text.pack c
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
    let toProcess = getProcessableFileList $ Text.unpack list
    processFiles =<< toProcess
doInclude x = return [x]
