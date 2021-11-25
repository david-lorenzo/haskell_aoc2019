import Control.Monad
import Data.Char
import Data.Function
import Data.List
import System.Environment


buildPatterns :: Int -> [[Int]]
buildPatterns n = map (take n . makePattern) $ [1..n]
  where makePattern n = [0, 1, 0, (-1)]       &   -- base pattern
                        map (take n . repeat) &   -- [a, b, c...] -> [[a, a], [b, b], [c, c], ...] when n = 2
                        join                  &   -- [a, a, b, b, c, c, ...]
                        repeat                &   -- [[a, a, b, b, c, c, ...], [a, a, b, b, c, c, ...], ...]
                        join                  &   -- [a, a, b, b, c, c, ..., a, a, b, b, c, c, ..., a, a, ...]
                        drop 1                    -- [a, b, b, c, c, ..., a, a, b, b, c, c, ..., a, a, ...]


fft :: [[Int]] -> [Int] -> [Int]
fft ps xs = zipWith mulAcc ps (repeat xs)
  where mulAcc xs ys = (`mod` 10) . abs . sum $ zipWith (*) xs ys


main = do
  inputFilePath:_ <- getArgs

  -- parsing input data
  inputData <- map digitToInt . filter isDigit <$> readFile inputFilePath

  let ps = buildPatterns . length $ inputData

  -- iterating 100 times the FFT function
  inputData               &   -- Input :: [Int]
    (iterate (fft ps))    &   -- Infinite structure :: [[Int]]
    drop 100              &   -- ignoring first 100 values
    head                  &   -- the solution is the next element :: [Int]
    take 8                &   -- the solution is the first 8 digits
    map show              &   -- converting the digits to string [String]
    join                  &   -- :: String
    putStrLn
