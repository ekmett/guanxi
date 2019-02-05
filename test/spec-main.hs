module Main where

import Test.Hspec.Runner
import Test.Hspec.Formatters
import qualified Cover.DLXSpec
import qualified FD.MonadSpec
import qualified Prompt.IteratorSpec

main :: IO ()
main = hspecWith defaultConfig {configFormatter = Just progress} $ do
  Cover.DLXSpec.spec
  FD.MonadSpec.spec
  Prompt.IteratorSpec.spec
