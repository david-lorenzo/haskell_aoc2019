import Data.Char
import Data.List
import System.Environment

getLayers :: Int -> String -> [String]
getLayers layerSize [] = []
getLayers layerSize string =
  let (layer, rest) = splitAt layerSize string
  in layer:(getLayers layerSize rest)


countChar :: Char -> String -> Int
countChar c = length . filter (== c)


comp :: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
comp (a, b, c) (d, e, f) = if a < d
                            then (a, b, c)
                            else (d, e, f)


mixColor :: Char -> Char -> Char
mixColor '2' a = a
mixColor a   b = a


mixLayers :: String -> String -> String
mixLayers lA lB = zipWith mixColor lA lB

drawPixel :: Char -> Char
drawPixel '0' = ' '
drawPixel '1' = '#'

main = do
  let width = 25
  let height = 6
  let layerSize = width * height
  inputFilePath <- head <$> getArgs
  inputData <- (takeWhile isDigit) <$> readFile inputFilePath
  -- demo input:
--   let width = 2
--   let height = 2
--   let layerSize = width * height
--   let inputData = "0222112222120000"
  let layers = getLayers layerSize inputData
  let result = foldl1' mixLayers layers
--   print $ map drawPixel result
--   print "By rows"
  mapM_ putStrLn $ getLayers width $ map drawPixel result
