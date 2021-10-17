import System.Environment

run1 :: Int -> Int
run1 x = (x `div` 3) - 2

main = do
  inputFilePath <- head <$> getArgs
  inputLines <- map read . lines <$> readFile inputFilePath
  let res1 = sum . map run1 $ inputLines
  putStrLn . ("Part 1: " ++) . show $ res1

