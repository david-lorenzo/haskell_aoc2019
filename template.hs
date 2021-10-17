
main = do
  let inputFilePath = "inputXX.txt"
  inputLines <- lines <$> readFile inputFilePath
  mapM_ print inputLines
  let res = 0
  putStrLn . ("Result: " ++) . show $ res

