import Control.Monad
import Data.Char
import Data.List
import System.Environment

pattern = [0, 1, 0, (-1)]

makePattern n = drop 1 . join . repeat . join . map (take n . repeat) $ pattern

mulAcc :: [Int] -> [Int] -> Int
mulAcc xs ys = (`mod` 10) . abs . sum $ zipWith (*) xs ys


fft :: [Int] -> [Int]
fft xs = zipWith mulAcc (repeat xs) ps
  where n = length xs
        ps = map (take n . makePattern) [1..n]


main = do
  inputFilePath:_ <- getArgs

  -- parsing input data
  inputData <- (map digitToInt) . (filter isDigit) <$> readFile inputFilePath

  -- iterating 100 times the FFT function
  let res = head . drop 100 . (iterate fft) $ inputData
  putStrLn . join . map show . take 8 $ res
