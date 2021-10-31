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

main = do
  let width = 25
  let height = 6
  let layerSize = width * height
  inputFilePath <- head <$> getArgs
  inputData <- (takeWhile isDigit) <$> readFile inputFilePath
  let layers = getLayers layerSize inputData
  let zeros = map (countChar '0') layers
  let ones = map (countChar '1') layers
  let twos = map (countChar '2') layers
  let freqs = zip3 zeros ones twos
  let (z, o, t) = foldl1' comp freqs
  print $ o * t
