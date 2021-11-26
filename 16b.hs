{-# LANGUAGE TypeApplications #-}

import Data.Char
import Data.List
import System.Environment

-- This problem is not feasable to be solved with a generic approach
-- the input data is 650 bytes, 10000 times make it 6.5 MB
--
-- Trying the same approach as in 16a problem would create (6.5e6)^2
-- coeficients (many GB of RAM). On the other hand, doing all the
-- multiplications and sums required on each phase would take too long
--
-- This part has a trick hidden. We can exploit the fact that the first 7 digits
-- is the pointer to the solution, this pointer is located near the end of the
-- input vector.
--
-- At those positions the next values are calculated as the sum of all the
-- elements from the desired position until the end of the vector so we can
-- ignore everything that happens before the pointer of the solution.
--
-- In that region the coeficients look like this:
-- ... 1 1 1 1
-- ... 0 1 1 1
-- ... 0 0 1 1
-- ... 0 0 0 1
--
-- Assuming X is the input vector and Y is the output vector then
--      Y[n] = X[n] + X[n+1] + X[n+2] + ... + X[N-1]
-- where N is the length of the input vector

phase xs = scanl' (\x y -> (x + y) `mod` 10) 0 xs

main = do
  inputFilePath:_ <- getArgs

  inputData <- map digitToInt . filter isDigit <$> readFile inputFilePath

  let n = length inputData
  let pointer = read @Int . concatMap show . take 7 $ inputData
  let nLast = n * 10000 - pointer
  let numbers = take nLast . cycle . reverse $ inputData

  let solution = concatMap show $ take 8 $ reverse $ head $ drop 100 $ iterate phase numbers
  print solution
