
low  = 248345
high = 746315

digits :: Int -> [Int]
digits n = go [] n
  where go acc m
          | m < 10    = m:acc
          | otherwise = go ((m `rem` 10):acc) (m `div` 10)

increasing (x:[])   = True
increasing (x:y:zs) = if x > y then False else increasing (y:zs)

repeating (x:[])   = False
repeating (x:y:zs) = if x == y then True else repeating (y:zs)

main = do
  let res1 = length . filter repeating . filter increasing . map digits $ [low..high]
  putStrLn . ("Part 1: " ++) . show $ res1

