#!/usr/bin/env runhaskell

{-
The MIT License (MIT)

Copyright (c) 2015 Dániel Stein <daniel@stein.hu>

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be included
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
-}

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

Alternatively, use one of the following to increase all the header levels in the
included file. The first option is a shortcut for incrementing the level by 1.
The second demonstrates an increase of 2.

> ```include-indented

> ```{ .include header-change=2 }

If the file does not exist, it will be skipped completely. No warnings, no
residue, nothing. Putting an # as the first character in the line will make the
filter skip that file.

For now the nested includes only work for two levels, after that the source
will be inserted and not parsed.

Note: the metadata from the included source files are discarded.

-}

import           Control.Monad
import           Data.List
import           Control.Error (readMay, fromMaybe)
import           System.Directory

import           Text.Pandoc
import           Text.Pandoc.Error
import           Text.Pandoc.JSON
import           Text.Pandoc.Walk

stripPandoc :: Either PandocError Pandoc -> [Block]
stripPandoc p =
  case p of
    Left _ -> [Null]
    Right (Pandoc _ blocks) -> blocks

ioReadMarkdown :: String -> IO(Either PandocError Pandoc)
ioReadMarkdown content = return $! readMarkdown def content

getContent :: Int -> String -> IO [Block]
getContent changeInHeaderLevel file = do
  c <- readFile file
  p <- ioReadMarkdown c
  return $! stripPandoc (modifyHeaderLevelWith changeInHeaderLevel <$> p)

getProcessableFileList :: String -> IO [String]
getProcessableFileList list = do
  let f = lines list
  let files = filter (\x -> not $ "#" `isPrefixOf` x) f
  filterM doesFileExist files

processFiles :: Int -> [String] -> IO [Block]
processFiles changeInHeaderLevel toProcess =
  fmap concat (getContent changeInHeaderLevel `mapM` toProcess)

doInclude :: Block -> IO [Block]
doInclude (CodeBlock (_, classes, options) list)
  | "include" `elem` classes = do
    let toProcess = getProcessableFileList list
        changeInHeaderLevel = fromMaybe 0 $ readMay =<< "header-change" `lookup` options
    processFiles changeInHeaderLevel =<< toProcess
  | "include-indented" `elem` classes =
    doInclude $ CodeBlock ("", newClasses, newOptions) list
        where
            newClasses = ("include" :) . delete "include-indented" $ classes
            newOptions = ("header-change","1") : options
doInclude x = return [x]

modifyHeaderLevelBlock :: Int -> Block -> Block
modifyHeaderLevelBlock n (Header int att inls) = Header (int + n) att inls
modifyHeaderLevelBlock _ x = x

modifyHeaderLevelWith :: Int -> Pandoc -> Pandoc
modifyHeaderLevelWith n = walk (modifyHeaderLevelBlock n)

main :: IO ()
main = toJSONFilter doInclude
