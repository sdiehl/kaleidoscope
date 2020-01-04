{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
) where

import qualified Parser
import qualified Codegen

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> pure () -- repl
    [fname] -> pure () -- file
