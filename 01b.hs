import System.Environment

run1 :: Int -> Int
run1 x = (x `div` 3) - 2

run2 :: Int -> Int
run2 = sum . takeWhile (>0) . tail . iterate run1

main = do
  inputFilePath <- head <$> getArgs
  inputLines <- map read . lines <$> readFile inputFilePath
  let res2 = sum . map run2 $ inputLines
  putStrLn . ("Part 2: " ++) . show $ res2

