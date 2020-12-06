#!/usr/bin/env runhaskell

import Text.Pandoc.JSON

import Text.Pandoc.Filter.Include

main :: IO ()
main = toJSONFilter doInclude
