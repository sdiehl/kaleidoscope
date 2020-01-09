{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main,
    debug,
  )
where

import qualified Codegen
import qualified JIT
import qualified Parser
import System.Directory
import System.Environment

--interactive :: Codegen ()
--interactive = runInputT defaultSettings loop
--  where
--    loop :: InputT Codegen ()
--    loop = getInputLine "ready> "

debug :: IO ()
debug = do
  contents <- readFile "src/chapter6/chapter6.k"
  let mast = Parser.parseToplevel contents
  case mast of
    Left err -> print err
    Right mod -> do
      lmod <- Codegen.evalCodegen (Codegen.codegenModule mod)
      res <- JIT.runJIT lmod
      --print res
      putStrLn "Done."
      pure ()

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> pure () -- repl
    [fname] -> do
      exists <- doesFileExist fname
      if exists
        then do
          contents <- readFile fname
          let mast = Parser.parseToplevel contents
          case mast of
            Left err -> print err
            Right mod -> do
              lmod <- Codegen.evalCodegen (Codegen.codegenModule mod)
              res <- JIT.runJIT lmod
              print res
              putStrLn "Done."
              pure ()
        else putStrLn "File does not exist"
