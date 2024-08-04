module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import FileSystem as FS

main :: Effect Unit
main = do
  FS.write "tmp.txt" "hello world" # launchAff_

