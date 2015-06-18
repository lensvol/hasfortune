import System.Environment
import System.IO
import System.Random

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

main = do
  (datFileName:_) <- getArgs
  contents <- readFile datFileName
  let fortunes = parseFortuneFile contents
  randomOne <- getRandomFortune fortunes
  putStrLn $ randomOne
