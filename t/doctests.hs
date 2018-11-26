-----------------------------------------------------------------------------
-- |
-- Module      :  Main (doctests)
-- Copyright   :  (C) 2012-18 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  provisional
-- Portability :  portable
--
-- This module provides doctests for a project based on the actual versions
-- of the packages it was built with. It requires a corresponding Setup.lhs
-- to be added to the project
-----------------------------------------------------------------------------
module Main where

import Build_doctests (flags, pkgs, module_sources)
import System.Environment (getArgs)
import Test.DocTest (doctest)

main :: IO ()
main = do
  args <- getArgs
  doctest $ flags ++ pkgs ++ module_sources ++ ["--fast"] ++ args
