{-# LANGUAGE OverloadedStrings #-}

module Main (
  main,
) where

import Parser
import Codegen

import Control.Monad.Trans

import System.IO
import System.Environment
import System.Console.Haskeline

import qualified LLVM.AST as AST

main :: IO ()
main = do
  args <- getArgs
  case args of
    []      -> pure () -- repl
    [fname] -> pure () -- file
