{-# LANGUAGE OverloadedStrings #-}

module HaskLox
  ( mainFunc,
  )
where

import Control.Monad (unless)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
  ( hFlush,
    isEOF,
    stdout,
  )

mainFunc :: IO ()
mainFunc = do
  args <- getArgs
  case args of
    [] -> runPrompt
    [filename] -> runFile filename
    _ -> putStrLn "Usage: hasklox [script]" >> exitFailure

runPrompt :: IO ()
runPrompt = do
  TIO.putStr "> "
  hFlush stdout
  eof <- isEOF
  unless eof $ do
    line <- TIO.getLine
    run line
    runPrompt

runFile :: FilePath -> IO ()
runFile filePath = do
  fileContents <- TIO.readFile filePath
  run fileContents

run :: T.Text -> IO ()
run _ = return ()