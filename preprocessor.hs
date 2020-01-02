{-# LANGUAGE OverloadedStrings #-}

import Prelude hiding (readFile, writeFile, getContents)
import Text.Read
import Data.Text as T
import Data.Text.IO as T
import Control.Monad.State

import Text.Pandoc

slice :: Int -> Int -> [Text] -> [Text]
slice from to xs = Prelude.take (to - from + 1) (Prelude.drop from xs)

doInclude :: Block -> IO Block
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "include" namevals of
     Just f -> do
        contents <- T.readFile (T.unpack f)
        return (CodeBlock (id, classes, namevals) contents)
     Nothing -> return cb
doInclude x = return x

main :: IO ()
main = do
  contents <- getContents
  out <- runIO $ do
    doc <- readMarkdown def contents
    included <- liftIO (bottomUpM doInclude doc)
    writeMarkdown def included
  case out of
    Left err -> print err
    Right out -> T.putStrLn out
