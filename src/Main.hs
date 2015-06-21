{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Applicative ((<|>))
import Control.Monad.Trans (liftIO)

import qualified Data.ByteString.Char8 as BSS

import System.Environment
import System.IO
import System.Random

import Snap.Core
import Snap.Http.Server


type Fortune = [String]

parseFortuneFile :: String -> [Fortune]
parseFortuneFile contents = splitOn "%" (lines contents)

splitOn x ys =
    let (y, ys') = break (== x) ys
    in  if null ys'
        then [y]
        else y : splitOn x (tail ys')

getRandomFortune :: [Fortune] -> IO String
getRandomFortune fortunes = do
  randomNum <- getStdRandom (randomR (0, length fortunes))
  return $ unlines (fortunes !! randomNum)


writeFortune :: [Fortune] -> Snap ()
writeFortune fortunes = do
  randomOne <- liftIO $ getRandomFortune fortunes
  writeBS (BSS.pack randomOne)

main = do
  (datFileName:_) <- getArgs
  contents <- readFile datFileName
  let fortunes = parseFortuneFile contents
  quickHttpServe
    $ ifTop (writeFortune fortunes)
