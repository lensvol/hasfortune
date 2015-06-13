import Data.List

import System.Environment
import System.IO
import System.Random

type Fortune = [String]

parseFortuneFile :: String -> [Fortune]
parseFortuneFile contents = splitOn "%" (lines contents)

splitOn :: (Eq a) => a -> [a] -> [[a]]
splitOn delim [] = []
splitOn delim mainList = splitOn' delim mainList [] []

splitOn' :: (Eq a) =>  a -> [a] -> [a] -> [[a]] -> [[a]]
splitOn' delim (x:xs) acc result
  | null xs = result ++ [acc ++ [x]]
  | x == delim = splitOn' delim xs [] (result ++ [acc])
  | otherwise = splitOn' delim xs (acc ++ [x]) result

main = do -- print $ splitOn "%" ["1", "2", "3", "%", "4", "5", "6"]
  (datFileName:_) <- getArgs
  contents <- readFile datFileName
  let fortunes = parseFortuneFile contents
  randomNum <- getStdRandom (randomR (0, length fortunes))
  putStrLn $ unlines  (fortunes !! randomNum)
