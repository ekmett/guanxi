-- TODO: replace this with auto-discovery
module Main where

import Test.Hspec.Api.Formatters.V1 (progress, useFormatter)
import Test.Hspec.Runner (defaultConfig, hspecWith)
import qualified Spec.Cover.DLX
import qualified Spec.FD.Monad
import qualified Spec.Domain.Interval
import qualified Spec.Prompt.Iterator
import qualified Spec.Logic.Reflection
import qualified Spec.Unaligned.Base

main :: IO ()
main = hspecWith (useFormatter ("progress", progress) defaultConfig) $ do
  Spec.Cover.DLX.spec
  Spec.Domain.Interval.spec
  Spec.FD.Monad.spec
  Spec.Logic.Reflection.spec
  Spec.Prompt.Iterator.spec
  Spec.Unaligned.Base.spec
