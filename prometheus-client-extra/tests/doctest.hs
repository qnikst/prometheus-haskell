module Main where

import Build_doctests (flags, module_sources, pkgs)
import Test.DocTest

main :: IO ()
main = do
  doctest args
  where args = flags ++ pkgs ++ module_sources
